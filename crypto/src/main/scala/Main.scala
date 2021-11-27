import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.allText
import spray.json._

import java.io.BufferedWriter
import java.io.FileWriter
import scala.collection.mutable.ListBuffer

//класс Криптовалюта
case class Cryptocurrence(name: String, ticker: String,price: Double, h24: Double,
                          h7: Double, market_cap: Long, volume: Long, circulating_supply: Long)


object MyJsonProtocol extends DefaultJsonProtocol {
  implicit val cryptocurrenceFormat = jsonFormat8(Cryptocurrence)
}
import MyJsonProtocol._

object Main {
  def main(args: Array[String]): Unit = {

    val path ="src/test/resources/"
    val buf = ListBuffer.empty[Cryptocurrence]
    val browser = JsoupBrowser()

    for( i <- 1 to 4) {
      val lists =parseCryptocurrence(browser,path + i + ".html")
      buf++=lists
    }
    val buf1 =buf.distinctBy(_.name)//удаление повторяющейся криптовалюты по имени
    println("Кол-во криптовалют в списке: "+ buf1.size)
    println(buf1.toList.toJson.prettyPrint)
    createJsonFile(buf1.toList,path)
    println("JSON файл создан")
  }


  def parseCryptocurrence(browser: Browser,str:String):List[Cryptocurrence]={
    val doc =browser.parseFile(str)
    val items_name = doc >> elementList("tbody .hKkaxT")
    val items_ticker = doc >> elementList("tbody .nhjPq")
    //списки имен и текеров
    val names = items_name.map(_ >> allText("p"))
    val tickers = items_ticker.map(_ >> allText("p"))

    val items_price = doc >> elementList("tbody .cLgOOr")
    //список цен
    val prices = items_price.map(_ >> allText("span")).map(_.replaceAll("[^0-9.]", "").toDouble)

    val items_change = doc >> elementList("tbody .sc-15yy2pl-0")
    val change_sign = items_change.map(_.toString).map(_.contains("icon-Caret-up"))
    val change_num = items_change.map(_ >> allText("span")).map(_.replaceAll("[^0-9.]", "").toDouble)
    val b = for ((sign,nums) <- change_sign.zip(change_num)) yield if (sign==false) nums*(-1)  else nums
    val (buf24, buf7) = (for(j <- 0 to (b.size-1)) yield if(j%2==0) (b(j),0) else (b(j),1)).toList.partition(x => x._2==0)
    //списки измений за 24 и 7 часов
    val h24s = buf24.map(_._1)
    val h7s = buf7.map(_._1)

    val items_market_cap = doc >> elementList("tbody .ieFnWP")
    //список рыночной капитализации
    val market_caps = items_market_cap.map(_ >> allText("span")).map(_.replaceAll("[^0-9]", "").toLong)

    val items_volume = doc >> elementList("tbody tr td:nth-child(8)") >?>elementList(".bRswdu.font_weight_500")
    val vol = items_volume.map(_ >> allText("p")).map(_.toList(0))
    //список обьемов(если значение отсутствует, то оно равняется 0)
    val volumes = for(el <- vol) yield if(el.isEmpty) 0 else el(0).replaceAll("[^0-9]", "").toLong

    val items_circ_sup = doc >> elementList("tbody .cTtqsY")
    //список циркулируещих предложений
    val circ_sups = items_circ_sup.map(_ >> allText("p")).map(_.replaceAll("[^0-9]", "").toLong)

    val lists = (for(el <- 0 to names.size-1) yield
      Cryptocurrence(names(el),tickers(el),prices(el),h24s(el),h7s(el),market_caps(el),volumes(el), circ_sups(el))).toList
    return lists
  }


  def createJsonFile(lists:List[Cryptocurrence],path:String): Unit =
  {
    val w = new BufferedWriter(new FileWriter(path+"output.json"))
    w.write(lists.toJson.prettyPrint)
    w.close
  }
}
