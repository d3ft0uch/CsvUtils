package crypto

import java.io.File
import java.util.Base64
import javax.crypto.Cipher
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}

import utils.Common

import scala.annotation.tailrec
import scala.io.Source
import scala.util.control.NonFatal


object DecryptAll {
  private var _source: String = _
  private var _target: String = _

  def main(args: Array[String]) {
    val arg = parseArgs(args, Args())
    if (!validateArgs(arg)) return usage
    val dir = arg.dir
    _source = dir
    val target = new File(new File(dir).getParent, "decrypted_CSV").getAbsolutePath
    Common.mkDir(new File(target))
    _target = target
    val key = arg.key
    println("Running..")
    println(arg.headers)
    Common.getFiles(new File(dir)).foreach(crypto(_, key, arg.headers))
    println("done")
    sys.exit(0)
  }

  def decrypt(key: String, value: String) = {
    // initializing vector
    val iv = new IvParameterSpec(Common.initVector.getBytes("UTF-8"))
    // initializing secret key for AES
    val skeySpec = new SecretKeySpec(getRepKey(key).getBytes("UTF-8"), "AES")
    // getting instance of AES cipher
    val cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING")
    // initializing the cipher
    cipher.init(Cipher.DECRYPT_MODE, skeySpec, iv)
    // ciphering prodecure
    val decrypted = cipher.doFinal(Base64.getDecoder.decode(value))
    // returning decrypted string
    new String(decrypted)
  }

  private def getRepKey(key: String): String = (if (key.length >= 16) key
  else key * (16 / key.length + 1)).substring(0, 16)

  private def usage() =
    println(
      "Usage: java -cp CsvUtils-assembly-1.0.jar " +
        "crypto.DecryptAll " +
        "-d {directory_ with_csv} -k {key} -h {header1};{header2}")

  private def getPath(src: String) = src.replace(_source, _target)

  private def crypto(file: File, key: String, headers: List[String]) = try {
    val data = Source.fromFile(file).getLines().toList match {
      case head :: tail =>
        val encIdx = headers.map(header => {
          val nonQueted = head.split(',').indexOf(header)
          if (nonQueted == -1) head.split(',').indexOf("\"" + header + "\"") else nonQueted
        })
        head :: tail.map {
          case line => line.split(",").zipWithIndex.map {
            case (value, idx) if encIdx.contains(idx) => decrypt(key, value)
            case (value, idx) => value
          } mkString ","
        } mkString "\n"
    }
    Common.save(new File(getPath(file.getAbsolutePath)), data)
  }
  catch {
    case NonFatal(ex) => // malformed data
  }

  @tailrec
  private def parseArgs(args: Array[String], parsed: Args): Args = {
    val options = List("-d", "-k", "-h")
    args.headOption match {
      case Some(opt) =>
        val value = args.tail.takeWhile(!options.contains(_))
        val next = opt match {
          case "-d" => parsed.copy(dir = value.mkString(" "))
          case "-k" => parsed.copy(key = value.mkString(" "))
          case "-h" => parsed.copy(headers = value.mkString.split('^').toList)
          case _ => parsed
        }
        parseArgs(args.tail, next)
      case None => parsed
    }
  }

  private def validateArgs(arg: Args) = arg.dir.nonEmpty && arg.key.nonEmpty && arg.headers.nonEmpty

  sealed case class Args(dir: String = "", key: String = "", headers: List[String] = List.empty)

}


