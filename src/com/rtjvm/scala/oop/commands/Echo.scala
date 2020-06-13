package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.files.{Directory, File}
import com.rtjvm.scala.oop.filesystem.State

class Echo(args: Array[String]) extends Command {

  override def apply(state: State): State = {
    /*
      if no args, state
      else if just one arg, print to consile
      else if multiple args
      {
        operator = next to last argument
        if >
          echo to a file (create a file if not there)
        if >>
         append to a file
        else
          just echo everything to console
      }
     */
    if (args.isEmpty) state
    else if (args.length == 1) state.setMessage(args(0))
    else {
      val operator = args(args.length - 2)
      val filename = args(args.length - 1)
      val contents = createContent(args, args.length - 2)

      if (">>".equals(operator))
        doEcho(state, contents, filename, append = true)
      else if (">".equals(operator))
        doEcho(state, contents, filename, append = false)
      else
        state.setMessage(createContent(args, args.length))
    }
  }

  def createContent(args: Array[String], topIndex: Int): String = {
    def createContentHelper(currentIndex: Int, accumulator: String): String = {
      if (currentIndex >= topIndex) accumulator
      else createContentHelper(currentIndex + 1, accumulator + " " + args(currentIndex))
    }
    createContentHelper(0, "")
  }

  def getRootAfterEcho(currDirectory: Directory, path: List[String], contents: String, append: Boolean): Directory = {
    if (path.isEmpty) currDirectory
    else if (path.tail.isEmpty) {
      val dirEntry = currDirectory.findEntry(path.head)
      if (dirEntry == null) currDirectory.addEntry(new File(currDirectory.path, path.head, contents))
      else if (dirEntry.isDirectory) currDirectory
      else
        if (append) currDirectory.replaceEntry(path.head, dirEntry.asFile.appendContents(contents))
        else currDirectory.replaceEntry(path.head, dirEntry.asFile.setContents(contents))
    } else {
      val nextDirectory = currDirectory.findEntry(path.head).asDirectory
      val newNextDirectory = getRootAfterEcho(nextDirectory, path.tail, contents, append)

      if (newNextDirectory == nextDirectory) currDirectory
      else currDirectory.replaceEntry(path.head, newNextDirectory)
    }
  }

  def doEcho(state: State, contents: String, filename: String, append: Boolean): State = {
    if (filename.contains(Directory.SEPARATOR))
      state.setMessage("Echo: filename must not contain separators")
    else {
      val newRoot: Directory = getRootAfterEcho(state.root, state.wd.getAllFoldersInPath :+ filename, contents, append)
      if (newRoot == state.root)
        state.setMessage(filename + ": no such file")
      else
        State(newRoot, newRoot.findDescendant(state.wd.getAllFoldersInPath))
    }
  }



}
