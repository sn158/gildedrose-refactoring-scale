package com.gildedrose

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class GildedRoseTest  extends AnyFunSpec with Matchers {
  it ("the quality of an item is never negative") {
    val items = Array[Item](new Item("Elixir of the Mongoose", 0, 0))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).quality should equal (0)
  }

  it("Quality cannot exceed 50, except Legendaty item") {
    val items = Array[Item](new Item("Aged Brie", 50, 50))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).quality should equal (50)
  }

  it ("should return the quality of Normal item") {
    val items = Array[Item](new Item("Elixir of the Mongoose", 0, 0))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).quality should equal (0)
  }

  it ("should correctly update Sulfaras, never degrades") {
    val items = Array[Item](new Item("Sulfuras, Hand of Ragnaros", 70, 80))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).quality should equal (80)
  }

  it ("should correctly update Sulfuras, never to be sold") {
    val items = Array[Item](new Item("Sulfuras, Hand of Ragnaros", 70, 80))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).sellIn should equal (70)
  }

  it ("should correctly update Normal items before expiration date") {
    val items = Array[Item](new Item("+5 Dexterity Vest", 10, 10))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).sellIn should equal (9)
  }

  it ("For expired item the quality degrades twice as fast") {
    val items = Array[Item](new Item("+5 Dexterity Vest", 0, 10))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).quality should equal (8)
  }

  it ("should correctly update Expired Normal item, degrade twice as fast") {
    val items = Array[Item](new Item("Elixir of the Mongoose", 0, 10))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).quality should equal (8)
  }

  it("should correctly update Aged Brie item") {
    val items = Array[Item](new Item("Aged Brie", 40, 40))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).quality should equal (41)
  }

  it("Backstage pass increase in quality as it ages")  {
    val items = Array[Item](new Item("Backstage passes to a TAFKAL80ETC concert", 40, 40))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).quality should equal (41)
  }

  it("Backstage pass increase by 2 quality when there are 10 days or less to sell") {
    val items = Array[Item](new Item("Backstage passes to a TAFKAL80ETC concert", 10, 10))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).quality should equal (12)
  }

  it("Backstage pass increase by 3 quality when there are 5 days or less to sell") {
    val items = Array[Item](new Item("Backstage passes to a TAFKAL80ETC concert", 5, 5))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).quality should equal (8)
  }

  it("Backstage pass quality drops to zero once sell by date arrives") {
    val items = Array[Item](new Item("Backstage passes to a TAFKAL80ETC concert", 0, 40))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).quality should equal (0)
  }

  it("should correctly update Conjured item, degrades by 2") {
    val items = Array[Item](new Item("Conjured Mana Cake", 40, 40))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).quality should equal (38)
  }

  it("Expired Conjured items degrade by 4") {
    val items = Array[Item](new Item("Conjured Mana Cake", -1, 40))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).quality should equal (36)
  }
}
