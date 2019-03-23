package com.example.auction.search

import akka.Done
import com.example.elasticsearch.IndexedItem
import com.example.elasticsearch.request.ItemQuery

import scala.concurrent.Future


trait IndexedStore[T] {

  def store(document : IndexedItem): Future[Done]

  def search(query: ItemQuery): Future[T]

}
