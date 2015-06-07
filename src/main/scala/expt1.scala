import org.joda.time._

object expt1 {

  def sorting( lista:List[scala.collection.immutable.Map[String,Any]] ): scala.collection.immutable.Map[String,Any] = {
    var mostRecentDate=(new DateTime).withYear(1970).withMonthOfYear(1).withDayOfMonth(1)
    var mergedOutput = Map( ("acc_id",1),("name","abc"),("count",1),("date",mostRecentDate) )
    for(currentMap<-lista)
      {
        var currentDate = DateTime.parse(currentMap.get("date").get.toString())
        var mergedDate = DateTime.parse(mergedOutput.get("date").get.toString())

        if( currentDate.isAfter(mergedDate) )
          {
            var keysOnlyInPrevMergedOutput = mergedOutput.keys.toSet diff currentMap.keys.toSet
            var valsOnlyInPrevMergedOutput = keysOnlyInPrevMergedOutput.map(key => mergedOutput(key))
            var keyValuesOnlyInPrevMergedOutput = keysOnlyInPrevMergedOutput.zip(valsOnlyInPrevMergedOutput).toMap

            var newOutput = currentMap ++ keyValuesOnlyInPrevMergedOutput
            mergedOutput = newOutput
          }
        else if(mergedDate.isAfter(currentDate))
          {
            var keysOnlyInCurrentMap = currentMap.keys.toSet diff mergedOutput.keys.toSet
            var valsOnlyInCurrentMap = keysOnlyInCurrentMap.map(key=>currentMap(key))
            var keyValuesOnlyInCurrentMap = keysOnlyInCurrentMap.zip(valsOnlyInCurrentMap).toMap

            var newOutput = mergedOutput ++ keyValuesOnlyInCurrentMap
            mergedOutput = newOutput
          }
      }
    println(mergedOutput)
    return mergedOutput
  }

  def main(args:Array[String]) {
    val mapADate=(new DateTime).withYear(2015).withMonthOfYear(2).withDayOfMonth(10)
    val mapBDate=(new DateTime).withYear(2015).withMonthOfYear(2).withDayOfMonth(12)
    val mapCDate=(new DateTime).withYear(2015).withMonthOfYear(2).withDayOfMonth(9)

    val mapA = Map( ("acc_id",123),("name","Jack"),("count",1),("date",mapADate),("city","slc") )
    val mapB = Map( ("acc_id",123),("name","John"),("count",10),("date",mapBDate) )
    val mapC = Map( ("acc_id",123),("count",20),("date",mapCDate),("title","Manager") )

    val lista=List(mapA,mapB,mapC)
    //println(lista)
    val a=sorting(lista)
    //println("Here's the merged output "+a)

  }
}

//expt1.main(Array())



