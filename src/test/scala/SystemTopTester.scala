import chisel3._
import chisel3.iotesters
import chisel3.iotesters.PeekPokeTester

import java.util

class SystemTopTester(dut: SystemTop) extends PeekPokeTester(dut) {
  //Do not run the accelerator
  poke(dut.io.start, 0)

  //Load the data memory with image data
  System.out.print("\nLoading the data memory with image data... ")
  //Uncomment one of the following line depending on the image you want to load to the data memory
  //var image = Images.blackImage
  //var image = Images.whiteImage
  var image = Images.cellsImage
  //var image = Images.borderCellsImage
  for( address <- 0 to image.length-1){
    poke(dut.io.testerDataMemEnable, 1)
    poke(dut.io.testerDataMemWriteEnable, 1)
    poke(dut.io.testerDataMemAddress, address)
    poke(dut.io.testerDataMemDataWrite, image(address))
    step(1)
  }
  poke(dut.io.testerDataMemEnable, 0)
  System.out.println("Done!")

  //Run the simulation of the CPU
  System.out.println("\nRun the simulation of the accelerator")
  //Start the CPU
  poke(dut.io.start, 1)
  var running = true
  var maxCycles = 20000
  var cyclesCounter = maxCycles
  while(running) {
    System.out.print("\rRunning cycle: " + (maxCycles - cyclesCounter))
    step(1)
    poke(dut.io.start, 0) //Start bit is 1 only for 1 cycle
    cyclesCounter = cyclesCounter - 1
    running = peek(dut.io.done) == 0 && cyclesCounter > 0
  }
  poke(dut.io.start, 0)
  System.out.println(" - Done!")

  //Dump the data memory content
  System.out.print("\nDump the data memory content... ")
  val inputImage = new util.ArrayList[Int]
  for( i <- 0 to 399){ //Location of the original image
    poke(dut.io.testerDataMemEnable, 1)
    poke(dut.io.testerDataMemWriteEnable, 0)
    poke(dut.io.testerDataMemAddress, i)
    val data = peek(dut.io.testerDataMemDataRead)
    inputImage.add(data.toInt)
    //System.out.println("a:" + i + " d:" + data )
    step(1)
  }
  val outputImage = new util.ArrayList[Int]
  for( i <- 400 to 799){ //Location of the processed image
    poke(dut.io.testerDataMemEnable, 1)
    poke(dut.io.testerDataMemWriteEnable, 0)
    poke(dut.io.testerDataMemAddress, i)
    val data = peek(dut.io.testerDataMemDataRead)
    outputImage.add(data.toInt)
    //System.out.println("a:" + i + " d:" + data )
    step(1)
  }
  poke(dut.io.testerDataMemEnable, 0)
  System.out.println("Done!")

  System.out.print("\r\n")
  System.out.println("Input image from address 0 to 399:")
  Images.printImage(inputImage)
  System.out.println("Processed image from address 400 to 799:")
  Images.printImage(outputImage)

  System.out.println("End of simulation")
}

object SystemTopTester {
  def main(args: Array[String]): Unit = {
    println("Testing the full system")
    iotesters.Driver.execute(
      Array("--generate-vcd-output", "on",
        "--target-dir", "generated",
        "--top-name", "SystemTop"),
      () => new SystemTop()) {
      c => new SystemTopTester(c)
    }
  }
}


