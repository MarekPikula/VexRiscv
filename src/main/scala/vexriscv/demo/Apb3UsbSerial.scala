package vexriscv.demo

import spinal.core._
import spinal.lib.{IMasterSlave, _}
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3SlaveFactory}
import spinal.lib.bus.misc.SingleMapping

/**
 * usb_serial and usb_phy SpinalHDL wrapper.
 *
 * usb_serial is a VHDL core by Joris van Rantwijk.
 * usb_phy is a Verilog core by Rudolf Usselmann.
 *
 * Author: Marek Pikuła, Trinamic 2019
 *   Date: 04.11.2019
 */

// usb_serial core
case class USBSerialConfig(vendorID: Bits, productID: Bits, versionBCD: Bits, versionStr: String = "",
                           productStr: String = "", serialStr: String = "", hsSupport: Boolean = false,
                           selfPowered: Boolean = false, rxBufSize: Int = 2048, txBufSize: Int = 1024) { }

class usb_serial(usbConfig: USBSerialConfig) extends BlackBox {
  val generic = new Generic {
    // Vendor ID to report in device descriptor. (16 bits)
    val VENDORID = usb_serial.this.usbConfig.vendorID

    // Product ID to report in device descriptor. (16 bits)
    val PRODUCTID = usb_serial.this.usbConfig.productID

    // Product version to report in device descriptor. (16 bits)
    val VERSIONBCD = usb_serial.this.usbConfig.versionBCD

    // Optional description of manufacturer (max 126 characters).
    val VENDORSTR = usb_serial.this.usbConfig.versionStr

    // Optional description of product (max 126 characters).
    val PRODUCTSTR = usb_serial.this.usbConfig.productStr

    // Optional product serial number (max 126 characters).
    val SERIALSTR = usb_serial.this.usbConfig.serialStr

    // Support high speed mode.
    val HSSUPPORT = usb_serial.this.usbConfig.hsSupport

    // Set to true if the device never draws power from the USB bus.
    val SELFPOWERED = usb_serial.this.usbConfig.selfPowered

    // Size of receive buffer as 2-logarithm of the number of bytes.
    // Must be at least 10 (1024 bytes) for high speed support.
    val RXBUFSIZE_BITS = log2Up(usb_serial.this.usbConfig.rxBufSize)

    // Size of transmit buffer as 2-logarithm of the number of bytes.
    val TXBUFSIZE_BITS = log2Up(usb_serial.this.usbConfig.txBufSize)
  }

  val io = new Bundle {
    // 60 MHz UTMI clock.
    val CLK = in Bool

    // Synchronous reset; clear buffers and re-attach to the bus
    val RESET = in Bool

    // High for one clock when a reset signal is detected on the USB bus.
    // Note: do NOT wire this signal to RESET externally.
    val USBRST = out Bool

    // High when the device is operating (or suspended) in high speed mode.
    val HIGHSPEED = out Bool

    // High while the device is suspended.
    // Note: This signal is not synchronized to CLK.
    // It may be used to asynchronously drive the UTMI SuspendM pin.
    val SUSPEND = out Bool

    // High when the device is in the Configured state.
    val ONLINE = out Bool

    // High if a received byte is available on RXDAT.
    val RXVAL = out Bool

    // Received data byte, valid if RXVAL is high.
    val RXDAT = out Bits(8 bits)

    // High if the application is ready to receive the next byte.
    val RXRDY = in Bool

    // Number of bytes currently available in receive buffer.
    val RXLEN = out Bits(log2Up(usbConfig.rxBufSize) bits)

    // High if the application has data to send.
    val TXVAL = in Bool

    // Data byte to send, must be valid if TXVAL is high.
    val TXDAT = in Bits(8 bits)

    // High if the entity is ready to accept the next byte.
    val TXRDY = out Bool

    // Number of free byte positions currently available in transmit buffer.
    val TXROOM = out Bits(log2Up(usbConfig.txBufSize) bits)

    // Temporarily suppress transmissions at the outgoing endpoint.
    // This gives the application an oppertunity to fill the transmit
    // buffer in order to blast data efficiently in big chunks.
    val TXCORK = in Bool

    // PHY
    val PHY = new Bundle {
      val DATAIN     = in  Bits(8 bits)
      val DATAOUT    = out Bits(8 bits)
      val TXVALID    = out Bool
      val TXREADY    = in  Bool
      val RXACTIVE   = in  Bool
      val RXVALID    = in  Bool
      val RXERROR    = in  Bool
      val LINESTATE  = in  Bits(2 bits)
      val OPMODE     = out Bits(2 bits)
      val XCVRSELECT = out Bool
      val TERMSELECT = out Bool
      val RESET      = out Bool
    }
  }

  mapClockDomain(clock = io.CLK, reset = io.RESET)
  addTag(noNumericType)
  noIoPrefix()
}

// usb_phy core
class usb_phy() extends BlackBox {
  val io = new Bundle {
    val clk         = in Bool
    val rst         = in Bool
    val phy_tx_mode = in Bool
    val usb_rst     = out Bool
    val txdp        = out Bool
    val txdn        = out Bool
    val txoe        = out Bool
    val rxd         = in Bool
    val rxdp        = in Bool
    val rxdn        = in Bool
    val DataOut_i   = in Bits(8 bits)
    val TxValid_i   = in Bool
    val TxReady_o   = out Bool
    val DataIn_o    = out Bits(8 bits)
    val RxValid_o   = out Bool
    val RxActive_o  = out Bool
    val RxError_o   = out Bool
    val LineState_o = out Bits(2 bits)
  }

  mapClockDomain(clock = io.clk, reset = io.rst)
  noIoPrefix()
}

// USB PHY interface between usb_phy and external transceiver
case class UsbPhy() extends Bundle with IMasterSlave {
  val OEn     = Bool
  val RCV     = Bool
  val VP      = Bool
  val VM      = Bool
  val SUSPND  = Bool
  val MODE    = Bool
  val SPEED   = Bool
  val VPO     = Bool
  val VMO     = Bool
  val SOFTCON = Bool

  override def asMaster(): Unit = {
    in(RCV, VP, VM)
    out(OEn, SUSPND, MODE, SPEED, VPO, VMO, SOFTCON)
  }
}

// APB3 configuration
object Apb3UsbSerial {
  def getApb3Config = Apb3Config(
    addressWidth  = 16,
    dataWidth     = 32,
    selWidth      = 1,
    useSlaveError = false
  )
}

// APB3 peripheral
case class Apb3UsbSerial(usbConfig : USBSerialConfig) extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(Apb3UsbSerial.getApb3Config))
    val usb = master(UsbPhy())
    val interrupt = out Bool
  }

  // Control signals to cores
  val ctrl = new Area {
    val txCork    = RegInit(False)  // (Serial) Temporarily suppress transmissions at the outgoing endpoint.
    val softCon   = RegInit(False)  // (PHY) Software-controlled USB connection. A HIGH level applies 3.3 V to pin Vpu(3.3).
    val phyTxMode = RegInit(False)  // (PHY) Selects the PHY Transmit Mode. Consult datasheet for details.
  }

  // Clock domains
  val utmiClockDomain = ClockDomain.external("utmi", frequency = FixedFrequency(60 MHz))
  val phyClockDomain  = ClockDomain.external("usb_phy", frequency = FixedFrequency(48 MHz))

  // CDC between usb_serial and usb_phy
  val utmi = new Area {
    val tx = new StreamCCByToggle(
      dataType = Bits(8 bits),
      inputClock = utmiClockDomain,
      outputClock = phyClockDomain
    )
    val rx = new FlowCCByToggle(
      dataType = Bits(8 bits),
      inputClock = phyClockDomain,
      outputClock = utmiClockDomain
    )
    val rxActive  = False
    rxActive.allowOverride

    val rxError   = False
    rxError.allowOverride

    val lineState = B"00"
    lineState.allowOverride
  }

  // CDC between APB and usb_serial
  val rxStream = new StreamCCByToggle(
    dataType    = Bits(8 bits),
    inputClock  = utmiClockDomain,
    outputClock = ClockDomain.current
  )

  val txFlow = new FlowCCByToggle(
    dataType    = Bits(8 bits),
    inputClock  = ClockDomain.current,
    outputClock = utmiClockDomain
  )

  // usb_serial instantiation within separate clock domain
  val utmiClockArea = new ClockingArea(utmiClockDomain) {
    val usbSerial = new usb_serial(usbConfig)

    val apbArea = new Area {
      // Input signals to core
      val txCork = BufferCC(ctrl.txCork, False)
      usbSerial.io.TXCORK <> txCork

      // RX/TX data streams
      val rx = new Stream(Bits(8 bits))
      usbSerial.io.RXDAT <> rx.payload
      usbSerial.io.RXRDY <> rx.ready
      usbSerial.io.RXVAL <> rx.valid
      rxStream.io.input << rx

      val tx = new Flow(Bits(8 bits))
      usbSerial.io.TXDAT <> tx.payload
      usbSerial.io.TXVAL <> tx.valid
      txFlow.io.output >> tx
    }

    val utmiArea = new Area {
      val rx = Flow(Bits(8 bits))
      rx.payload <> usbSerial.io.PHY.DATAIN
      rx.valid   <> usbSerial.io.PHY.RXVALID
      utmi.rx.io.output >> rx

      val tx = Stream(Bits(8 bits))
      tx.payload <> usbSerial.io.PHY.DATAOUT
      tx.ready   <> usbSerial.io.PHY.TXREADY
      tx.valid   <> usbSerial.io.PHY.TXVALID
      utmi.tx.io.input << tx

      usbSerial.io.PHY.RXACTIVE  <> BufferCC(utmi.rxActive, False)
      usbSerial.io.PHY.RXERROR   <> BufferCC(utmi.rxError, False)
      usbSerial.io.PHY.LINESTATE <> BufferCC(utmi.lineState, B"00")
    }
  }

  val phyClockArea : ClockingArea = new ClockingArea(phyClockDomain) {
    val usbPhy = new usb_phy()

    // Input signals to core
    val phyTxMode = BufferCC(ctrl.phyTxMode, False)
    val softCon   = BufferCC(ctrl.softCon,   False)

    val utmiArea = new Area {
      val rx = new Flow(Bits(8 bits))
      rx.payload <> usbPhy.io.DataIn_o
      rx.valid   <> usbPhy.io.RxValid_o
      utmi.rx.io.input << rx

      val tx = new Stream(Bits(8 bits))
      tx.payload <> usbPhy.io.DataOut_i
      tx.valid   <> usbPhy.io.TxValid_i
      tx.ready   <> usbPhy.io.TxReady_o
      utmi.tx.io.output >> tx

      utmi.rxActive  <> usbPhy.io.RxActive_o
      utmi.rxError   <> usbPhy.io.RxError_o
      utmi.lineState <> usbPhy.io.LineState_o
      phyTxMode      <> usbPhy.io.phy_tx_mode
    }

    val phyArea = new Area {
      io.usb.OEn     <> usbPhy.io.txoe
      io.usb.RCV     <> usbPhy.io.rxd
      io.usb.VP      <> usbPhy.io.rxdp
      io.usb.VM      <> usbPhy.io.rxdn
      io.usb.SUSPND  := False //<> utmiClockArea.usbSerial.io.SUSPEND
      io.usb.MODE    <> phyTxMode
      io.usb.SPEED   := True // FS, since usb_phy only supports FS
      io.usb.VPO     <> usbPhy.io.txdp
      io.usb.VMO     <> usbPhy.io.txdn
      io.usb.SOFTCON <> softCon
    }
  }

  // Status signals from core
  val usbRst    = BufferCC(utmiClockArea.usbSerial.io.USBRST,    False)
  val highSpeed = BufferCC(utmiClockArea.usbSerial.io.HIGHSPEED, False)
  val suspend   = BufferCC(utmiClockArea.usbSerial.io.SUSPEND,   False)
  val online    = BufferCC(utmiClockArea.usbSerial.io.ONLINE,    False)
  val rxLen     = BufferCC(utmiClockArea.usbSerial.io.RXLEN,     B(0, log2Up(usbConfig.rxBufSize) bits))
  val txRoom    = BufferCC(utmiClockArea.usbSerial.io.TXROOM,    B(0, log2Up(usbConfig.txBufSize) bits))
  val txRdy     = BufferCC(utmiClockArea.usbSerial.io.TXRDY,     False)

  // Interrups TODO
  io.interrupt := False

  // APB
  val apbArea = new Area {
    val bus = Apb3SlaveFactory(io.apb)

    // 0x00 STATUS register
    // For flag descriptions consult usb_serial signal documentation above
    bus.read(0x0,
      0 -> usbRst,
      1 -> highSpeed,
      2 -> suspend,
      3 -> online,
      4 -> txRdy)

    // 0x04 CTRL register
    bus.driveAndRead(ctrl.txCork, 0x4, 0)
    bus.driveAndRead(ctrl.softCon, 0x4, 1)
    bus.driveAndRead(ctrl.phyTxMode, 0x4, 2)

    // 0x08 INT_STAT – interrupt status register
    // 0x0C INT_MASK – interrupt mask register

    // FIFO_STATUS register
    bus.read(0x10,
      0 -> rxLen,
      16 -> txRoom)

    // RX FIFO interface
    bus.readStreamNonBlocking(rxStream.io.output, 0x14)

    // TX FIFO interface
    bus.driveFlow(txFlow.io.input, 0x18)
  }
}