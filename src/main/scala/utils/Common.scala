package utils

import java.io.{File, PrintWriter}

object Common {

   val initVector = "qweaslfkjclaasda"

   def save(output: File, data: String) = {
     mkDir(new File(output.getParent))
     val pw = new PrintWriter(output)
     pw.write(data)
     pw.close()
   }

    def mkDir(file: File) = {
     try {
       file.mkdirs()
     }
     catch {
       case e: SecurityException => throw new SecurityException(s"Cannot make directory ${file.getName}. Access denied.")
     }
   }

   def getFiles(f: File): Array[File] = {
     val these = f.listFiles
     these.filter(_.isFile) ++ these.filter(_.isDirectory).flatMap(getFiles)
   }

   def trimExtension(str: String) = str.substring(0, str.lastIndexOf('.'))
 }
