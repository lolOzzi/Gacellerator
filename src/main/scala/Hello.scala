/*
 * Hello.scala
 *
 * This is just a simple circuit that reads a binary image stored in the data memory (address 0 to 399),
 * inverts the pixel color (black becomes white and viceversa) and write the image in the data memory
 * (address 400 to 799).
 *
 * The associated tester for this circuit is test/scala/HelloTester.scala. There you can select the image you want
 * to use as input. To speed-up simulation images are 20x20 pixels. Images are stored in test/scala/Images.scala.
 *
 * Run in the terminal>> sbt "test:runMain HelloTester" to see it at work and test your Chisel3/Scala/Java
 * installation. In the Hello
 *
 * Author: Luca Pezzarossa
 */

import chisel3._
import chisel3.util._

class Hello extends Module {
  val io = IO(new Bundle {
    val done = Output(Bool ())
    val start = Input(Bool ())
    //The following signals are used by the tester to load and dump the memory contents. Do not touch.
    val testerDataMemEnable = Input(Bool ())
    val testerDataMemAddress = Input(UInt (16.W))
    val testerDataMemDataRead = Output(UInt (32.W))
    val testerDataMemWriteEnable = Input(Bool ())
    val testerDataMemDataWrite = Input(UInt (32.W))
  })

  //Creating components
  val dataMemory = Module(new DataMemory())

  //State enum and register
  val idle :: read :: write :: done :: Nil = Enum (4)
  val stateReg = RegInit(idle)

  //Support registers
  val addressReg = RegInit(0.U(16.W))
  val dataReg = RegInit(0.U(32.W))

  //Default values
  dataMemory.io.writeEnable := false.B
  dataMemory.io.address := 0.U(16.W)
  dataMemory.io.dataWrite := dataReg
  io.done := false.B

  //FSMD switch
  switch(stateReg) {
    is(idle) {
      when(io.start) {
        stateReg := read
        addressReg := 0.U(16.W)
      }
    }

    is(read) {
      dataMemory.io.address := addressReg
      dataReg := Cat(0.U(24.W), ~dataMemory.io.dataRead(7,0))
      stateReg := write
    }

    is(write) {
      dataMemory.io.address := addressReg + 400.U(16.W)
      dataMemory.io.writeEnable := true.B
      addressReg := addressReg + 1.U(16.W)
      when(addressReg === 399.U(16.W)) {
        stateReg := done
      } .otherwise{
        stateReg := read
      }
    }

    is(done) {
      io.done := true.B
      stateReg := done
    }
  }

  //This signals are used by the tester for loading and dumping the data memory content, do not touch
  dataMemory.io.testerAddress := io.testerDataMemAddress
  io.testerDataMemDataRead := dataMemory.io.testerDataRead
  dataMemory.io.testerDataWrite := io.testerDataMemDataWrite
  dataMemory.io.testerEnable := io.testerDataMemEnable
  dataMemory.io.testerWriteEnable := io.testerDataMemWriteEnable

}

