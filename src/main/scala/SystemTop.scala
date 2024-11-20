import chisel3._
import chisel3.util._

class SystemTop extends Module {
  val io = IO(new Bundle {
    val done = Output(Bool ())
    val start = Input(Bool ())

    val testerDataMemEnable = Input(Bool ())
    val testerDataMemAddress = Input(UInt (16.W))
    val testerDataMemDataRead = Output(UInt (32.W))
    val testerDataMemWriteEnable = Input(Bool ())
    val testerDataMemDataWrite = Input(UInt (32.W))

  })

  //Creating components
  val dataMemory = Module(new DataMemory())
  val accelerator = Module(new Accelerator())


  //Connect done and start
  io.done := accelerator.io.done
  accelerator.io.start := io.start

  //Connect data memory and accelerator
  accelerator.io.dataRead := dataMemory.io.dataRead
  dataMemory.io.address := accelerator.io.address
  dataMemory.io.dataWrite := accelerator.io.dataWrite
  dataMemory.io.writeEnable := accelerator.io.writeEnable

  //This signals are used by the tester for loading and dumping the data memory content, do not touch
  dataMemory.io.testerAddress := io.testerDataMemAddress
  io.testerDataMemDataRead := dataMemory.io.testerDataRead
  dataMemory.io.testerDataWrite := io.testerDataMemDataWrite
  dataMemory.io.testerEnable := io.testerDataMemEnable
  dataMemory.io.testerWriteEnable := io.testerDataMemWriteEnable

}