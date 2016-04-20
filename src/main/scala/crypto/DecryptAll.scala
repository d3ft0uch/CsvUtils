package crypto

import java.io.File
import java.util.Base64
import javax.crypto.Cipher
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}

import utils.Common

import scala.io.Source
import scala.util.control.NonFatal


object DecryptAll {
  // vector for some randomness
  private var _source: String = _
  private var _target: String = _

  def main(args: Array[String]) {
    if (args.length < 2) return usage
    val dir = args.dropRight(1).mkString(" ")
    _source = dir
    val target = (dir.split(File.separator).dropRight(1) ++ Array("decrypted_CSV")) mkString File.separator + File.separator
    Common.mkDir(new File(target))
    _target = target
    val key = args.takeRight(1).head
    println("Running..")
    Common.getFiles(new File(dir)).foreach(crypto(_, key))
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

  private def usage() = println("Usage: java -cp CsvUtils-assembly-1.0.jar crypto.DecryptAll {directory_with_csv} {key}")

  private def getPath(src: String) = src.replace(_source, _target)

  private def crypto(file: File, key: String) = try {
    val data = Source.fromFile(file).getLines().map {
      case line => line.split(",").zipWithIndex.map {
        case (value, idx) if idx == 1 => decrypt(key, value)
        case (value, idx) if idx == 4 => decrypt(key, value)
        case (value, idx) => value
      } mkString ","
    } mkString "\n"
    Common.save(new File(getPath(file.getAbsolutePath)), data)
  }
  catch {
    case NonFatal(ex) => // malformed data
  }
}


