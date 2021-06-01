package com.typesafe.slick.testkit.tests

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.global
import com.typesafe.slick.testkit.util.{AsyncTest, RelationalTestDB}

class MappedColumnTest extends AsyncTest[RelationalTestDB] {
  import tdb.profile.api._


  case class IGeoShape( lat: Double )
  object IGeoShape {
    def applyOption(from: Option[String]): Option[IGeoShape] =
      from.map(deserialize)
    def unapplyOption(gsOpt: Option[IGeoShape]): Option[Option[String]] = {
      val jsonOpt = for (gs <- gsOpt) yield {
        serialize( gs )
      }
      Some(jsonOpt)
    }
    def deserialize(str: String): IGeoShape =
      IGeoShape( str.toDouble )
    def serialize( gs: IGeoShape ): String =
      gs.lat.toString
  }


  case class TheItem(
                      id: Option[Long] = None,
                      gs: Option[IGeoShape] = None,
                    )


  class TheItemsTable(tag: Tag) extends Table[TheItem](tag, "MappedTest0") {

    def id = column[Option[Long]]( "id" )
    def geoShapeStringOption = column[Option[String]]( "geo_shape" )

    def geoShapeOption = geoShapeStringOption <> (IGeoShape.applyOption, IGeoShape.unapplyOption)

    override def * = (id, geoShapeOption) <> ( (TheItem.apply _).tupled, TheItem.unapply )

  }

  lazy val theItemsQuery = TableQuery[TheItemsTable]


  def test = Future {
    theItemsQuery
      //.filter(_.id >=  0L)
      .filter(_.geoShapeStringOption.nonEmpty )
      .map(_.geoShapeOption)
      .distinct
      .result
  }

}
