trait Cell

class EmptyCell() extends Cell {
  override def toString: String = "empty"
}

class NumberCell(number: Int) extends Cell {
  override def toString: String = number.toString;
}

class StringCell(string: String) extends Cell {
  override def toString: String = string;
}

class ReferenceCell(ix: Int, iy: Int, table:Table) extends Cell {
  override def toString: String = {
    val nextCell = table.getCell(ix, iy)
    nextCell match {
      case None => "outOfRange"
      case Some(cell: ReferenceCell) => if (cell.getNextCell.get == this) "cyclic" else cell.getNextCell.get.toString
      case Some(cell: Cell) => cell.toString
    }
  }

  def getNextCell: Option[Cell] = {
    return table.getCell(ix, iy)
  }
}
