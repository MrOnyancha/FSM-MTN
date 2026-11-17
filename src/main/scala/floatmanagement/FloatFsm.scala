package floatmanagement

import akka.actor.{Actor, ActorLogging, Props}

/** Pure state transitions. All amounts are non-negative integer currency minor units. */
object FloatFsm {
  sealed trait Kind
  case object FloatRequest extends Kind
  case object CashRequest extends Kind

  sealed trait Status
  case object Requested extends Status
  case object Approved extends Status
  case object Assigned extends Status
  case object HandoverConfirmed extends Status
  case object TransferPending extends Status
  case object OutcomeUnknown extends Status
  case object TransferFailed extends Status
  case object Completed extends Status
  case object Cancelled extends Status

  final case class BalanceSnapshot(wallet: String, amount: Long, currency: String, observedAt: Long)
  final case class Request(id: String, kind: Kind, agent: String, owner: String,
      amount: Long, currency: String, status: Status = Requested,
      worker: Option[String] = None, transferId: Option[String] = None,
      before: Vector[BalanceSnapshot] = Vector.empty, after: Vector[BalanceSnapshot] = Vector.empty)
  final case class Worker(id: String, owner: String, limit: Long, cash: Long,
      cashReserved: Long = 0L, capacityReserved: Long = 0L, pendingDeposit: Long = 0L) {
    def availableCash: BigInt = BigInt(cash) - cashReserved - pendingDeposit
    def capacity: BigInt = BigInt(limit) - cash - capacityReserved
  }
  final case class Deposit(id: String, worker: String, amount: Long, acknowledged: Boolean = false)
  final case class State(requests: Map[String, Request] = Map.empty,
      workers: Map[String, Worker] = Map.empty, deposits: Map[String, Deposit] = Map.empty,
      processed: Map[String, Action] = Map.empty)

  sealed trait Action
  final case class RegisterWorker(worker: Worker) extends Action
  final case class CreateRequest(request: Request) extends Action
  final case class Approve(requestId: String) extends Action
  final case class Assign(requestId: String, workerId: String) extends Action
  final case class ConfirmHandover(requestId: String) extends Action
  final case class StartTransfer(requestId: String, transferId: String) extends Action
  final case class RecordBalance(requestId: String, snapshot: BalanceSnapshot, after: Boolean) extends Action
  final case class TransferSucceeded(requestId: String, transferId: String) extends Action
  final case class TransferRejected(requestId: String, transferId: String) extends Action
  final case class TransferOutcomeUnknown(requestId: String, transferId: String) extends Action
  final case class Cancel(requestId: String) extends Action
  final case class DeclareDeposit(id: String, workerId: String, amount: Long) extends Action
  final case class AcknowledgeDeposit(id: String) extends Action

  final case class Command(id: String, action: Action)
  final case class Result(state: State, duplicate: Boolean)

  def applyCommand(s: State, c: Command): Either[String, Result] = {
    if (c.id.trim.isEmpty) Left("Command ID is required")
    else s.processed.get(c.id) match {
      case Some(previous) if previous == c.action => Right(Result(s, duplicate = true))
      case Some(_) => Left("Command ID was already used for a different action")
      case None => transition(s, c.action).map(next => Result(next.copy(processed = next.processed.updated(c.id, c.action)), duplicate = false))
    }
  }

  private def transition(s: State, a: Action): Either[String, State] = {
    def request(id: String): Either[String, Request] = s.requests.get(id).toRight("Unknown request")
    def worker(id: String): Either[String, Worker] = s.workers.get(id).toRight("Unknown worker")
    def update(r: Request): State = s.copy(requests = s.requests.updated(r.id, r))
    def updateBoth(r: Request, w: Worker): State = update(r).copy(workers = s.workers.updated(w.id, w))
    def requireState(ok: Boolean, error: String): Either[String, Unit] = if (ok) Right(()) else Left(error)
    a match {
      case RegisterWorker(w) =>
        for {
          _ <- requireState(w.id.nonEmpty && w.owner.nonEmpty && w.limit >= 0 && w.cash >= 0 && w.cash <= w.limit && w.cashReserved == 0 && w.capacityReserved == 0 && w.pendingDeposit == 0, "Invalid worker")
          _ <- requireState(!s.workers.contains(w.id), "Worker already exists")
        } yield s.copy(workers = s.workers.updated(w.id, w))
      case CreateRequest(r) =>
        for {
          _ <- requireState(r.id.nonEmpty && r.agent.nonEmpty && r.owner.nonEmpty && r.currency.nonEmpty && r.amount > 0 && r.status == Requested && r.worker.isEmpty && r.transferId.isEmpty && r.before.isEmpty && r.after.isEmpty, "Invalid request")
          _ <- requireState(!s.requests.contains(r.id), "Request already exists")
        } yield update(r)
      case Approve(id) => request(id).flatMap(r => requireState(r.status == Requested, "Request is not awaiting approval").map(_ => update(r.copy(status = Approved))))
      case Assign(id, wid) => for {
        r <- request(id)
        w <- worker(wid)
        _ <- requireState(r.status == Approved && r.owner == w.owner, "Worker or request is not eligible")
        _ <- requireState(if (r.kind == FloatRequest) w.capacity >= r.amount else w.availableCash >= r.amount, "Insufficient cash or collection capacity")
        nw = if (r.kind == FloatRequest) w.copy(capacityReserved = w.capacityReserved + r.amount) else w.copy(cashReserved = w.cashReserved + r.amount)
      } yield updateBoth(r.copy(status = Assigned, worker = Some(wid)), nw)
      case ConfirmHandover(id) => for {
        r <- request(id)
        _ <- requireState(r.status == Assigned, "Request is not assigned")
        w <- worker(r.worker.get)
        nw = if (r.kind == FloatRequest) w.copy(cash = w.cash + r.amount, capacityReserved = w.capacityReserved - r.amount) else w.copy(cash = w.cash - r.amount, cashReserved = w.cashReserved - r.amount)
      } yield updateBoth(r.copy(status = HandoverConfirmed), nw)
      case StartTransfer(id, tid) => for {
        r <- request(id)
        _ <- requireState(r.status == HandoverConfirmed && tid.nonEmpty && !s.requests.values.exists(_.transferId.contains(tid)), "Invalid or duplicate transfer")
      } yield update(r.copy(status = TransferPending, transferId = Some(tid)))
      case RecordBalance(id, snap, after) => for {
        r <- request(id)
        _ <- requireState(snap.amount >= 0 && snap.wallet.nonEmpty && snap.currency == r.currency && snap.observedAt > 0, "Invalid balance snapshot")
        _ <- requireState(if (after) r.status == Completed else r.status == Requested || r.status == Approved || r.status == Assigned || r.status == HandoverConfirmed, "Snapshot not allowed in this state")
      } yield update(if (after) r.copy(after = r.after :+ snap) else r.copy(before = r.before :+ snap))
      case TransferSucceeded(id, tid) => transferResult(s, id, tid, Completed)
      case TransferRejected(id, tid) => transferResult(s, id, tid, TransferFailed)
      case TransferOutcomeUnknown(id, tid) => transferResult(s, id, tid, OutcomeUnknown)
      case Cancel(id) => for {
        r <- request(id)
        _ <- requireState(Set[Status](Requested, Approved, Assigned).contains(r.status), "Cannot cancel after handover")
        next <- r.worker match {
          case None => Right(update(r.copy(status = Cancelled)))
          case Some(wid) => worker(wid).map { w =>
            val nw = if (r.kind == FloatRequest) w.copy(capacityReserved = w.capacityReserved - r.amount) else w.copy(cashReserved = w.cashReserved - r.amount)
            updateBoth(r.copy(status = Cancelled), nw)
          }
        }
      } yield next
      case DeclareDeposit(id, wid, amount) => for {
        w <- worker(wid)
        _ <- requireState(id.nonEmpty && !s.deposits.contains(id) && amount > 0 && w.availableCash >= amount, "Invalid deposit")
      } yield s.copy(workers = s.workers.updated(wid, w.copy(pendingDeposit = w.pendingDeposit + amount)), deposits = s.deposits.updated(id, Deposit(id, wid, amount)))
      case AcknowledgeDeposit(id) => for {
        d <- s.deposits.get(id).toRight("Unknown deposit")
        _ <- requireState(!d.acknowledged, "Deposit already acknowledged")
        w <- worker(d.worker)
      } yield s.copy(workers = s.workers.updated(w.id, w.copy(cash = w.cash - d.amount, pendingDeposit = w.pendingDeposit - d.amount)), deposits = s.deposits.updated(id, d.copy(acknowledged = true)))
    }
  }

  private def transferResult(s: State, id: String, tid: String, target: Status): Either[String, State] =
    s.requests.get(id).toRight("Unknown request").flatMap { r =>
      if (r.transferId.contains(tid) && (r.status == TransferPending || r.status == OutcomeUnknown))
        Right(s.copy(requests = s.requests.updated(id, r.copy(status = target))))
      else Left("Unmatched transfer or invalid state")
    }
}

/** In-memory actor wrapper for local development only. Do not use for financial operations. */
final class FloatActor extends Actor with ActorLogging {
  import FloatFsm._
  private var state = State()
  def receive: Receive = {
    case cmd: Command =>
      val reply = sender()
      applyCommand(state, cmd) match {
        case Right(result) => state = result.state; reply ! Right(result)
        case Left(error) => reply ! Left(error)
      }
  }
}
object FloatActor { def props: Props = Props(new FloatActor) }
