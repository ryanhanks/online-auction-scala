package controllers

import com.example.auction.search.api.{SearchRequest, SearchService, SearchItem}
import com.example.auction.user.api.UserService
import com.example.auction.utils.PaginatedSequence
import play.api.data.Forms.nonEmptyText
import play.api.data.{Form, Mapping}
import play.api.data.Forms._
import play.api.data.{Form, FormError}
import play.api.mvc._
import play.api.mvc.ControllerComponents

import scala.concurrent.{ExecutionContext, Future}

class SearchController(
                        searchService: SearchService,
                        userService: UserService,
                        controllerComponents: ControllerComponents)(implicit ec: ExecutionContext)
  extends AbstractAuctionController(userService, controllerComponents) {
  def searchForm() = Action.async { implicit rh =>
    requireUser(loadNav(_).map { implicit nav =>
      println(SearchItemsForm.form.fill(SearchItemsForm()))
      val ps = PaginatedSequence(Seq[SearchItem](), 0, 0, 0)
      Ok(views.html.searchItems(showInlineInstruction = true, form = SearchItemsForm.form.fill(SearchItemsForm()), results = ps))
      //      Ok(views.html.editItem(ItemForm.fill(ItemForm())))
    })
  }

  def search() = Action.async { implicit request =>
    requireUser(user =>
      loadNav(user).flatMap { implicit nav =>
        SearchItemsForm.form.bindFromRequest().fold(errorForm => {
          val ps = PaginatedSequence(Seq[SearchItem](), 0, 0, 0)

          Future.successful(Ok(views.html.searchItems(showInlineInstruction = true, form = errorForm, results = ps)))
        }
          , searchItemsForm => {
            val r = searchService.search(0, 100)
              .invoke(SearchRequest(Some(searchItemsForm.keywords), Some(searchItemsForm.maximumPrice.intValue()), Some(searchItemsForm.currency.name)))
              .map(searchResponse => {
                val items = PaginatedSequence(searchResponse.items, searchResponse.pageNo, searchResponse.pageSize, searchResponse.numResults)
                println(searchResponse.items)
                Ok(views.html.searchItems(showInlineInstruction = true, form = SearchItemsForm.form.fill(searchItemsForm), results = items))
              })
            r
//            val p = Ok(views.html.searchItems(showInlineInstruction = true, form = SearchItemsForm.form.fill(searchItemsForm)))
//            p
          })
      })
  }
}


case class SearchItemsForm(keywords: String = "",
                           //                           itemStatus: String,
                           //                           category: String,
                           //                           userName: String,
                           maximumPrice: BigDecimal = 0.0,
                           currency: Currency = Currency.USD,
                           pageNumber: Int = 0)

object SearchItemsForm {
  val currency: Mapping[Currency] = nonEmptyText
    .verifying("invalid.currency", c => Currency.isDefined(c))
    .transform[Currency](Currency.valueOf, _.name)
  val form = Form(mapping(
    "keywords" -> text,
    "maximumPrice" -> bigDecimal,
    "currency" -> currency,
    "pageNumber" -> number(min = 0)
  )(SearchItemsForm.apply)(SearchItemsForm.unapply))


}

