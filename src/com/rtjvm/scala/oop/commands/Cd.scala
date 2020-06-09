package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

import scala.annotation.tailrec

class Cd(dir: String) extends Command {

  override def apply(state: State): State = {
    /*
      cd /something/somethingElse/../
      cd a/b/c = relative to the current working directory

      cd ..
      cd .
      cd a/./.././a/
     */

    // 1. find the root
    val root = state.root
    val wd = state.wd

    // 2. find the absolute path of the directory I want to cd to
    val absolutePath = {
      if (dir.startsWith(Directory.SEPARATOR)) dir
      else if (wd.isRoot) wd.path + dir
      else wd.path + Directory.SEPARATOR + dir
    }

    // 3. find the directory to cd to, given the path
    val destinationDirectory = doFindEntry(root, absolutePath)

    // 4. change the state given the new direcotry
    if (destinationDirectory == null || !destinationDirectory.isDirectory)
      state.setMessage(dir + ": no such directory")
    else State(root, destinationDirectory.asDirectory)
  }

  def doFindEntry(root: Directory, path: String): DirEntry = {
    @tailrec
    def findEntryHelper(currDirectory: Directory, tokens: List[String]): DirEntry = {
      if (tokens.isEmpty || tokens.head.isEmpty) currDirectory
      else if (path.tail.isEmpty) currDirectory.findEntry(tokens.head)
      else {
        val nextDir = currDirectory.findEntry(tokens.head)
        if (nextDir == null || !nextDir.isDirectory) null
        else findEntryHelper(nextDir.asDirectory, tokens.tail)
      }
    }

    // 1. tokens
    val tokens: List[String] = path.substring(1).split(Directory.SEPARATOR).toList

    // 2. navigate to the correct entry
    findEntryHelper(root, tokens)
  }
}
