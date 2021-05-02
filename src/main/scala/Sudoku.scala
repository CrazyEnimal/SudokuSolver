import scala.annotation.tailrec

trait SudokuSolver {
  def solve(input: Seq[String]): Seq[String]
}

object Sudoku extends SudokuSolver {

  override def solve(input: Seq[String]): Seq[String] = {
    val field = Field.init(input)
    val simpleTry: Field = simpleSolve(field)
    if(simpleTry.isSolved)simpleTry.toSeq
    else bruteforceSolve(field).toSeq
  }

  def manualSolve(input: Seq[String]): Field = {
    val field = Field.init(input)
    val simpleTry: Field = simpleSolve(field)
    if(simpleTry.isSolved)simpleTry
    else bruteforceSolve(field)
  }

  private def nextCells(field: Field): Seq[Cell] = {
    field.cells
      .filter(_.forecast.nonEmpty)
      .sortWith(_.forecast.length < _.forecast.length)
  }

  private def simpleSolve(field: Field): Field = {

    @tailrec
    def loop(inputField: Field): Field = {
      val next = nextCells(inputField)
      if(next.isEmpty) inputField
      else loop(Field.doStepWithProcess(inputField, next.head))
    }

    loop(field)
  }

  private def bruteforceSolve(field: Field): Field = {
    val sortedCells = nextCells(field)

    @tailrec
    def loopForecasts(inputForecasts: List[Int], solveForecasts: Seq[Field], cell: Cell): Seq[Field] = {
      if(inputForecasts.isEmpty || solveForecasts.head.isSolved) solveForecasts
      else {
        val doCell = cell.copy(forecast = List[Int](inputForecasts.head))
        val doSolve = simpleSolve(Field.doStep(field, doCell))

        if(doSolve.isSolved) loopForecasts(inputForecasts.tail, doSolve +: solveForecasts, cell)
        else loopForecasts(inputForecasts.tail, solveForecasts, cell)
      }
    }

    @tailrec
    def loopCells(inputCells: Seq[Cell], solveCells: Seq[Field] = Seq(field)): Seq[Field] = {
      if(inputCells.isEmpty || solveCells.head.isSolved) solveCells
      else {
        val solvesForecast = loopForecasts(inputCells.head.forecast, solveCells, inputCells.head)

        if(solvesForecast == solveCells) loopCells(inputCells.tail, solveCells)
        else loopCells(inputCells.tail, solvesForecast ++ solveCells)
      }
    }

    val solves = loopCells(sortedCells)
    solves.filter(_.isSolved) match {
      case Nil => field
      case head :: _ => head
    }
  }
}

sealed case class Point(x: Int, y: Int)

sealed case class Block(coordinates: Point)

sealed case class Cell(coordinates: Point, number: Int, forecast: List[Int] = List.empty[Int]){
  lazy val block: Block = {
    val x = ((coordinates.x -1) / 3)+1
    val y = ((coordinates.y -1) / 3)+1
    Block(Point(x,y))
  }

  private def nextCellCoordinates: Point =
    if(coordinates.x == 9) Point(1,coordinates.y + 1)
    else Point(coordinates.x + 1, coordinates.y)

  def getNextCell(n: Int): Cell = Cell(nextCellCoordinates, n, forecast)
}

sealed case class Field(cells: Seq[Cell]) {
  private def addCell(cell: Cell): Field = Field(cells :+ cell)

  def isSolved: Boolean = {
    cells.find(cell => cell.number == 0).getOrElse(None) match {
      case None => true
      case _ => false
    }
  }

  private def getBlockNumbers(block: Block): Seq[Int] = cells.filter(cell => cell.block == block && cell.number > 0).map(_.number)

  private def getRowAndColNumbers(point: Point): Seq[Int] = {
    cells.filter(cell =>
      (cell.coordinates.x == point.x || cell.coordinates.y == point.y) &&
        cell.coordinates != point &&
        cell.number > 0)
      .map(_.number)
  }

  private def getRowAndColForecasts(point: Point): Seq[Int] = (1 to 9).diff(getRowAndColNumbers(point))

  private def getCellForecasts(cell: Cell): List[Int] =
    getRowAndColForecasts(cell.coordinates).diff(getBlockNumbers(cell.block)).toList

  private def getCellSingletonForecasts(cell: Cell): Cell = {
    def isRowAndColForecasts(fn: Int, point: Point): Boolean = {
      val fByX = cells
        .filter(tCell => tCell.coordinates.x == point.x && tCell.coordinates != point).count(_.forecast.contains(fn))
      val fByY = cells
        .filter(tCell => tCell.coordinates.y == point.y && tCell.coordinates != point).count(_.forecast.contains(fn))
      if(fByX == 0 || fByY == 0) true
      else false
    }

    val forecast = cell.forecast.foldLeft(Seq.empty[Int])(
        (acc, fcEl) => if(isRowAndColForecasts(fcEl, cell.coordinates)) acc :+ fcEl else acc
      ) match {
        case Nil => 0
        case seq: Seq[Int] => if(seq.length == 1) seq.head else 0
      }

    if(forecast != 0) cell.copy(number = forecast, forecast = List.empty[Int])
    else cell
  }

  override def toString: String = {
    cells.foldLeft("")(
      (acc, cell) => if(cell.coordinates.x == 9) s"$acc${if(cell.number == 0) "-" else cell.number}\n" else s"$acc${if(cell.number == 0) "-" else cell.number}"
    )
  }

  def toSeq: Seq[String] = {
    toString()
      .split("\n")
  }
}

object Field {
  private def fillForecasts(field: Field): Field = {
    field.cells
      .foldLeft(Field(Seq.empty[Cell]))((acc, cell) => cell.number match {
        case 0 => acc.addCell(cell.copy(forecast = field.getCellForecasts(cell)))
        case _ => acc.addCell(cell.copy(forecast = List.empty[Int]))
      })
  }

  private def processSingletons(field: Field): Field = {
    field.cells
      .foldLeft(Field(Seq.empty[Cell]))((acc, cell) => cell.number match {
        case 0 => acc.addCell(field.getCellSingletonForecasts(cell))
        case _ => acc.addCell(cell.copy(forecast = List.empty[Int]))
      })
  }

  def doStepWithProcess(field: Field, cell: Cell): Field = {
    val newField = field.cells.foldLeft(Field(Seq.empty[Cell]))(
      (acc, nCell) =>
        if(cell.coordinates == nCell.coordinates)
          acc.addCell(nCell.copy(number = nCell.forecast.head, forecast = nCell.forecast.tail))
        else acc.addCell(nCell)
    )

    @tailrec
    def loop(f1: Field, f2: Field): Field = {
      if(f1 == f2) f2
      else loop(f2, fillForecasts(processSingletons(f1)))
    }

    loop(fillForecasts(processSingletons(fillForecasts(newField))), fillForecasts(newField))
  }

  def doStep(field: Field, cell: Cell): Field = {
    val newField = field.cells.foldLeft(Field(Seq.empty[Cell]))(
      (acc, nCell) =>
        if(cell.coordinates == nCell.coordinates) acc.addCell(nCell.copy(number = cell.forecast.head))
        else acc.addCell(nCell)
    )

    fillForecasts(newField)
  }

  def init(inputSeq: Seq[String]): Field = {
    sealed case class Acc(field: Field = Field(Seq.empty[Cell]), prevCell: Cell)

    val initField = inputSeq.flatMap(_.toString)
      .map(ch => if(ch == '-') '0' else ch)
      .map(_.toString.toInt)
      .foldLeft(Acc(prevCell = Cell(Point(0,1),0)))(
        (acc, num) => Acc(acc.field.addCell(acc.prevCell.getNextCell(num)), acc.prevCell.getNextCell(num))
      ).field

    fillForecasts(initField)
  }
}