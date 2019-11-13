package vexriscv.demo

import spinal.core._
import spinal.lib._
import spinal.core.{B, BlackBox, Bool, Bundle, Component, Generic, in, out}
import spinal.lib.bus.amba3.apb.Apb3
import spinal.lib.{IMasterSlave, master, slave}

case class CAN() extends Bundle with IMasterSlave {
  val tx = Bool
  val rx = Bool

  override def asMaster(): Unit = {
    out(tx)
    in(rx)
  }
}

class CTU_CAN_FD_v1_0(rx_buffer_size: Int, sup_filtA: Boolean, sup_filtB: Boolean, sup_filtC: Boolean) extends BlackBox {
  // SpinalHDL will look at Generic classes to get attributes which should be used ad VHDL gererics / Verilog parameter
  // You can use String Int Double Boolean and all SpinalHDL base types as generic value
  val generic = new Generic {
    val rx_buffer_size = CTU_CAN_FD_v1_0.this.rx_buffer_size
    val sup_filtA      = CTU_CAN_FD_v1_0.this.sup_filtA
    val sup_filtB      = CTU_CAN_FD_v1_0.this.sup_filtB
    val sup_filtC      = CTU_CAN_FD_v1_0.this.sup_filtC
  }

  // Define io of the VHDL entiry / Verilog module
  val io = new Bundle {
    val aclk = in Bool
    val arst = in Bool

    val irq       = out Bool
    val CAN_tx    = out Bool
    val CAN_rx    = in  Bool
    val timestamp = in  UInt(64 bit)

    val s_apb = new Bundle {
      val paddr   = in  Bits(32 bit)
      val penable = in  Bool
      val pprot   = in  Bits(3 bit)
      val prdata  = out Bits(32 bit)
      val pready  = out Bool
      val psel    = in  Bits(1 bit)
      val pslverr = out Bool
      val pstrb   = in  Bits(4 bit)
      val pwdata  = in  Bits(32 bit)
      val pwrite  = in  Bool
    }
  }

  noIoPrefix()
  mapCurrentClockDomain(clock = io.aclk, reset = io.arst)
}

class CTU_CAN_FD_v1_0_apb(rx_buffer_size: Int, sup_filtA: Boolean, sup_filtB: Boolean, sup_filtC: Boolean) extends Component {
  val io = new Bundle {
    val irq = out Bool
    val can = master(CAN())
    val timestamp = in UInt(64 bit)

    val apb = slave(Apb3(
      addressWidth = 16,
      dataWidth = 32
    ))
  }

  val can = new CTU_CAN_FD_v1_0(
    rx_buffer_size = this.rx_buffer_size,
    sup_filtA      = this.sup_filtA,
    sup_filtB      = this.sup_filtB,
    sup_filtC      = this.sup_filtC
  )

  can.io.s_apb.paddr   := B"h0031" ## io.apb.PADDR
  can.io.s_apb.penable := io.apb.PENABLE
  can.io.s_apb.prdata  <> io.apb.PRDATA
  can.io.s_apb.pready  <> io.apb.PREADY
  can.io.s_apb.psel    <> io.apb.PSEL
  can.io.s_apb.pslverr <> io.apb.PSLVERROR
  can.io.s_apb.pwdata  <> io.apb.PWDATA
  can.io.s_apb.pwrite  <> io.apb.PWRITE
  can.io.s_apb.pstrb   := B"hF"
  can.io.s_apb.pprot   := B"000"

  can.io.irq       <> io.irq
  can.io.timestamp <> io.timestamp
  can.io.CAN_rx    <> io.can.rx
  can.io.CAN_tx    <> io.can.tx
}