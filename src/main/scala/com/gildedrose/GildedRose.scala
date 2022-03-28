package com.gildedrose

class GildedRose(val items: Array[Item]) {

  def updateQuality() {
    items.foreach{ item => {
      val itemUpdater = itemMatcher(item.name) match {
        case "Normal" => Normal
        case "Aged" => Aged
        case "Legendary" => Legendary
        case "BackstagePass" => BackstagePass
        case "Conjured" => Conjured
      }

      itemUpdater.updateQuality(item)
    }
    }
  }

  val itemMatcher: Map[String, String] =
    Map(
      "+5 Dexterity Vest" -> "Normal",
      "Elixir of the Mongoose" -> "Normal",
      "Aged Brie" -> "Aged",
      "Sulfuras, Hand of Ragnaros" -> "Legendary",
      "Backstage passes to a TAFKAL80ETC concert" -> "BackstagePass",
      "Conjured Mana Cake" -> "Conjured"
    )

  trait ProductType {
    def updateQuality(item: Item)
  }

  case object Normal extends ProductType {
    override def updateQuality(item: Item): Unit = {
      val decreaseRate = if (item.sellIn <= 0) 2 else 1
      item.quality = decrease(item.quality, decreaseRate)
      item.sellIn -= 1
    }
  }

  case object Aged extends ProductType {
    override def updateQuality(item: Item): Unit = {
      item.quality = increase(item.quality, 1, 50)
      item.sellIn -= 1
    }
  }

  case object Legendary extends ProductType {
    override def updateQuality(item: Item): Unit = {}
  }

  object BackstagePass extends ProductType {
    override def updateQuality(item: Item): Unit = {
      val rate = {
        if (item.sellIn > 10) {
          1
        } else if (item.sellIn <=10 && item.sellIn > 5) {
          2
        } else if (item.sellIn <= 5 && item.sellIn > 0) {
          3
        } else 0
      }
      item.quality = if (item.sellIn > 0) {
        increase(item.quality, rate, 50)
      } else { 0 }
      item.sellIn -= 1
    }
  }

  case object Conjured extends ProductType {
    override def updateQuality(item: Item): Unit = {
      val decreaseRate = if (item.sellIn <= 0) 4  else 2
      item.quality = decrease(item.quality, decreaseRate)
      item.sellIn -= 1
    }
  }

  private def decrease(qualityValue: Int, rate: Int): Int = {
    val newQuality = qualityValue - rate
    if (newQuality >= 0) {
      newQuality
    } else 0
  }

  private def increase(qualityValue: Int, rate: Int, threshold: Int): Int = {
    val newQuality = qualityValue + rate
    if (newQuality <= threshold) {
      newQuality
    } else threshold
  }

}