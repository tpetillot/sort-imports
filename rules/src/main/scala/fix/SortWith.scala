package fix

import scala.meta.Import

sealed trait ImportOrdering extends Ordering[Import] {

  protected def strFirstImport(imp: Import): String =
    imp.children.head.syntax
}

class DefaultSort extends ImportOrdering {

  override def compare(x: Import, y: Import): Int =
    strFirstImport(x).compareTo(strFirstImport(y))
}

class WildcardAndGroupFirstSort extends ImportOrdering {

  private def transformForSorting(imp: Import): (String, String) = {
    val strImp = strFirstImport(imp)
    (strImp, strImp.replaceAll("_", "\0").replaceAll("\\{.+\\}", "\1"))
  }

  override def compare(x: Import, y: Import): Int =
    (transformForSorting(x), transformForSorting(y)) match {
      case ((strImp1, tranformedStrImp1), (strImp2, tranformedStrImp2)) =>
        val transformComparison = tranformedStrImp1.compareTo(tranformedStrImp2)
        if (transformComparison != 0) transformComparison else strImp1.compareTo(strImp2)
    }
}
