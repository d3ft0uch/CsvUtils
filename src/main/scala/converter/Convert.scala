package converter

import java.io.{File, FileInputStream, PrintWriter}

import org.apache.poi.hssf.usermodel.HSSFWorkbook
import org.apache.poi.ss.usermodel.Cell
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import utils.Common

import scala.collection.JavaConversions._
import scala.io.Source
import scala.util.control.NonFatal
import scala.xml.XML

object Convert {
  val delimitter = ","
  private var _source: String = _
  private var _target: String = _

  def main(args: Array[String]) {
    if (args.length < 1) return usage
    val dir = args.mkString(" ")
    _source = dir
    val target = (dir.split(File.separator).dropRight(1) ++ Array("CSV")) mkString File.separator + File.separator
    _target = target
    println("Running..")
    mkDir(new File(target))
    val files = Common.getFiles(new File(dir))
    // Chain of responsibility pattern
    val csv = new CSVHandler(None)
    val tsv = new TSVHandler(Some(csv))
    val html = new HtmlHandler(Some(tsv))
    val xls = new XLSHandler(Some(html))
    val first = new XLSXHandler(Some(xls))
    files.foreach(first.process)
    println("done")
    sys.exit(0)
  }


  def escapeTags(str: String) = str
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll("&lt;thead", "<thead")
    .replaceAll("&lt;/thead", "</thead")
    .replaceAll("&lt;tr", "<tr")
    .replaceAll("&lt;/tr", "</tr")
    .replaceAll("&lt;td", "<td")
    .replaceAll("&lt;/td", "</td")
    .replaceAll("&lt;table", "<table")
    .replaceAll("&lt;/table", "</table")

  def escapeText(str: String) = "\"" + str + "\""

  private def usage() = println("Usage: java -cp CsvUtils-assembly-1.0.jar converter.Convert {path_to_directory}")

  abstract class Handler {

    def process(file: File): Unit

    val successor: Option[Handler]

  }

  class HtmlHandler(val successor: Option[Handler]) extends Handler {
    override def process(file: File): Unit = {
      try {
        val input = Source.fromFile(file).getLines().map(escapeTags).mkString
        val table = XML.loadString(input) \ "tr" map (_ \ "td")
        val data = table.map {
          case row => row.map(node => escapeText(node.text)).mkString(delimitter)
        } mkString "\n"
        save(new File(getPath(file.getAbsolutePath) + Common.trimExtension(file.getName) + ".csv"), data)
      }
      catch {
        case NonFatal(ex) => successor match {
          case Some(succ) => succ.process(file)
          case None => // do nothing if no successors remains
        }
      }
    }
  }

  class XLSHandler(val successor: Option[Handler]) extends Handler {
    override def process(file: File): Unit = {
      try {
        val workbook = new HSSFWorkbook(new FileInputStream(file))
        for (idx <- 0 to workbook.getNumberOfSheets - 1) {
          val sheet = workbook.getSheetAt(idx)
          val sheetName = sheet.getSheetName
          val data = sheet.iterator.map {
            case row => row.map(getCellValue) mkString delimitter
          } mkString "\n"
          save(new File(getPath(file.getAbsolutePath) + sheetName + ".csv"), normalize(data))
        }
      }
      catch {
        case NonFatal(ex) =>
          successor match {
            case Some(succ) =>
              succ.process(file)
            case None => // do nothing if no successors remains
          }
      }
    }
  }

  class XLSXHandler(val successor: Option[Handler]) extends Handler {
    override def process(file: File): Unit = {
      try {
        val workbook = new XSSFWorkbook(new FileInputStream(file))
        for (idx <- 0 to workbook.getNumberOfSheets - 1) {
          val sheet = workbook.getSheetAt(idx)
          val sheetName = sheet.getSheetName
          val data = sheet.iterator.map {
            case row => row.map(getCellValue) mkString delimitter
          } mkString "\n"
          save(new File(getPath(file.getAbsolutePath) + sheetName + ".csv"), normalize(data))
        }
      }
      catch {
        case NonFatal(ex) =>
          successor match {
            case Some(succ) =>
              succ.process(file)
            case None => // do nothing if no successors remains
          }
      }
    }
  }

  class TSVHandler(val successor: Option[Handler]) extends Handler {
    override def process(file: File): Unit = try {
      val input = Source.fromFile(file).getLines().toList
      val flag = validate(input)
      if (flag) {
        val data = input.map(_.replaceAll("\t", delimitter)) mkString "\n"
        save(new File(getPath(file.getAbsolutePath) + Common.trimExtension(file.getName) + ".csv"), data)
      }
      else successor match {
        case Some(succ) => succ.process(file)
        case None => // do nothing if no successors remains
      }
    }
    catch {
      case NonFatal(ex) => successor match {
        case Some(succ) =>
          succ.process(file)
        case None => // do nothing if no successors remains
      }
    }

    private def validate(lines: List[String]) = try {
      if (lines.nonEmpty) {
        val dimensions = lines.map(_.split("\t").length).toSet
        dimensions.size == 1 && !dimensions.contains(1)
      }
      else false
    }
    catch {
      case NonFatal(ex) => false
    }
  }

  class CSVHandler(val successor: Option[Handler]) extends Handler {

    override def process(file: File): Unit = try {
      val input = Source.fromFile(file).getLines().toList
      if (validate(input)) {
        save(new File(getPath(file.getAbsolutePath) + file.getName), input mkString "\n")
      }
      else successor match {
        case Some(succ) => succ.process(file)
        case None => // do nothing if no successors remains
      }
    }
    catch {
      case NonFatal(ex) => successor match {
        case Some(succ) => succ.process(file)
        case None => // do nothing if no successors remains
      }
    }

    private def validate(lines: List[String]) = try {
      if (lines.nonEmpty) {
        val dimensions = lines.map(_.split(delimitter).length).toSet
        dimensions.size == 1 && !dimensions.contains(1)
      }
      else false
    }
    catch {
      case NonFatal(ex) => false
    }
  }

  private def mkDir(file: File) = {
    try {
      file.mkdirs()
    }
    catch {
      case e: SecurityException => throw new SecurityException(s"Cannot make directory ${file.getName}. Access denied.")
    }
  }

  private def getPath(src: String) = {
    val path = Common.trimExtension(src.replace(_source, _target)) + File.separator
    path
  }


  private def save(output: File, data: String) = {
    mkDir(new File(output.getParent))
    val pw = new PrintWriter(output)
    pw.write(data)
    pw.close()
  }

  private def getCellValue(cell: Cell) = cell.getCellType match {
    case Cell.CELL_TYPE_BOOLEAN => cell.getBooleanCellValue.toString
    case Cell.CELL_TYPE_NUMERIC => cell.getNumericCellValue.toString
    case Cell.CELL_TYPE_STRING => cell.getStringCellValue
    case Cell.CELL_TYPE_BLANK => " "
    case Cell.CELL_TYPE_ERROR => cell.getErrorCellValue.toString
  }

  private def normalize(input: String) = {
    val lines = input.split('\n')
    val max = lines.map(_.split(delimitter).length).max
    lines.map {
      case line =>
        val values = line.split(delimitter)
        (values ++ Array.fill(max - values.length)(" ")).mkString(delimitter)
    } mkString "\n"
  }
}
