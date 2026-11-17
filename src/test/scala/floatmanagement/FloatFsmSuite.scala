package floatmanagement

import munit.FunSuite
import FloatFsm._

class FloatFsmSuite extends FunSuite {
  def run(s: State, id: String, a: Action): State = applyCommand(s, Command(id, a)).fold(e => fail(e), _.state)
  def setup(kind: Kind): State = {
    val w = run(State(), "worker", RegisterWorker(Worker("d", "sa", 100, 60)))
    val r = run(w, "create", CreateRequest(Request("r", kind, "agent", "sa", 30, "UGX")))
    run(r, "approve", Approve("r"))
  }
  test("float collects cash before transfer and does not complete before success") {
    val assigned = run(setup(FloatRequest), "assign", Assign("r", "d"))
    assertEquals(assigned.workers("d").capacityReserved, 30L)
    val received = run(assigned, "receipt", ConfirmHandover("r"))
    assertEquals(received.workers("d").cash, 90L)
    val pending = run(received, "transfer", StartTransfer("r", "tx"))
    assertEquals(pending.requests("r").status, TransferPending)
    assertEquals(run(pending, "success", TransferSucceeded("r", "tx")).requests("r").status, Completed)
  }
  test("cash reservations prevent double assignment and cancellation releases them") {
    val one = run(setup(CashRequest), "assign", Assign("r", "d"))
    val two = run(one, "create2", CreateRequest(Request("r2", CashRequest, "a2", "sa", 40, "UGX")))
    val approved = run(two, "approve2", Approve("r2"))
    assert(applyCommand(approved, Command("assign2", Assign("r2", "d"))).isLeft)
    val cancelled = run(approved, "cancel", Cancel("r"))
    assertEquals(cancelled.workers("d").cashReserved, 0L)
    assert(applyCommand(cancelled, Command("assign2", Assign("r2", "d"))).isRight)
  }
  test("unknown and failed outcomes preserve handed-over cash") {
    val assigned = run(setup(CashRequest), "assign", Assign("r", "d"))
    val handed = run(assigned, "receipt", ConfirmHandover("r"))
    val pending = run(handed, "transfer", StartTransfer("r", "tx"))
    val unknown = run(pending, "unknown", TransferOutcomeUnknown("r", "tx"))
    val failed = run(unknown, "failed", TransferRejected("r", "tx"))
    assertEquals(failed.workers("d").cash, 30L)
    assertEquals(failed.requests("r").status, TransferFailed)
    assert(applyCommand(failed, Command("cancel", Cancel("r"))).isLeft)
  }
  test("deposit does not restore capacity until acknowledgement") {
    val w = run(State(), "worker", RegisterWorker(Worker("d", "sa", 100, 100)))
    val declared = run(w, "declare", DeclareDeposit("dep", "d", 20))
    assertEquals(declared.workers("d").capacity, BigInt(0))
    assertEquals(declared.workers("d").availableCash, BigInt(80))
    val acknowledged = run(declared, "ack", AcknowledgeDeposit("dep"))
    assertEquals(acknowledged.workers("d").capacity, BigInt(20))
  }
  test("command redelivery does not repeat a handover") {
    val assigned = run(setup(FloatRequest), "assign", Assign("r", "d"))
    val cmd = Command("receipt", ConfirmHandover("r"))
    val first = applyCommand(assigned, cmd).toOption.get
    val repeat = applyCommand(first.state, cmd).toOption.get
    assert(repeat.duplicate)
    assertEquals(repeat.state.workers("d").cash, 90L)
    assert(applyCommand(first.state, Command("receipt", Cancel("r"))).isLeft)
  }
}
