import java.io.{File, PrintWriter}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Main {

  def main(args : Array[String]): Unit = {

    val path : String = "C:\\Users\\samuelexferri\\IdeaProjects\\Google Hash Code 2020\\src\\"
    //val file : String = path + "b_read_on.txt"
    //val file : String = path + "c_incunabula.txt"
    //val file : String = path + "d_tough_choices.txt"
    //val file : String = path + "e_so_many_books.txt"
    val file : String = path + "b.txt"


    val lines : List[String] = Source.fromFile(new File(file)).getLines.toList
    val configuration : String = lines(0)
    val score : List[Int] = lines(1).split("\\s").toList.map(_.toInt)

    val numbooks = configuration.split("\\s")(0)
    val numlibraries = configuration.split("\\s")(1)
    val numdays = configuration.split("\\s")(2)

    val libraries_info = lines.drop(2)

    var libraries : ArrayBuffer[Library] = ArrayBuffer[Library]()

    var i = 0
    var j = 0
    println(libraries_info)
    while(i < libraries_info.size - 1){

      val params : List[String] = libraries_info(i).split("\\s").toList
      val lbooks : List[Book] = libraries_info(i+1).split("\\s").toList.map(x => new Book(x.toInt, score))

      libraries.addOne(
        new Library(
          j,
          params(0).toInt,
          lbooks,
          params(1).toInt,
          params(2).toInt,
          ArrayBuffer[Int]()
        )
      )

      j += 1
      i += 2
    }

    /*for (l <- libraries) {
      l.books = l.books.sortBy(score(_).toInt)(Ordering.Int.reverse)
    }*/


    ///////////////////////////////////////////////////////////////////

    var inserted_books = ArrayBuffer[Int]()

    var hashmap : mutable.HashMap[Int,Int] = mutable.HashMap[Int,Int]()

    for(library <- libraries){
      var libscore = 0
      for(book <- library.books){
        libscore += score.apply(book.id).toInt
      }
      hashmap.addOne(library.ID, libscore)
    }

    //////////////////////////////////////////////////////////////////////////////////////////////
    def sorter(a: Library, b: Library): Boolean = {
      ((a.booksxday* hashmap.getOrElse(a.ID,0))/(a.signupdays*a.numbooks) ) > ((b.booksxday* hashmap.getOrElse(b.ID,0))/(b.signupdays*b.numbooks))
    }

    var RES_books_ID : ArrayBuffer[Int] = ArrayBuffer[Int]()
    var RES_libraries : ArrayBuffer[Library] = ArrayBuffer[Library]() // in ordine di sign up
    libraries = libraries.sortWith(sorter)

    println(libraries)

    ////////////////////////////////////////////////////////////////////////////////////////////////

    var deadline : Int = numdays.toInt
    var signup_time = 0
    i = 0

    while(deadline > 0 && i < libraries.size){
      if(deadline > libraries(i).signupdays){
        deadline -= libraries(i).signupdays
        signup_time += libraries(i).signupdays
        val remaining_days = numdays.toInt - signup_time- 1


        scala.util.Sorting.quickSort(libraries(i).books.toArray)
        j = 0
        while(j < libraries(i).books.size){

          if(!inserted_books.contains(libraries(i).books(j))){
            libraries(i).booksToSend.addOne(libraries(i).books(j).id)
            inserted_books.addOne(libraries(i).books(j).id)
          }
          j += 1
        }

        RES_libraries.addOne(libraries(i))
      }
      i += 1
    }



    ////////////////////////////////////////////////////////////////////////////////////////////////


    //println(RES_libraries)
    println(inserted_books.size + " " + inserted_books.distinct.size)


    ///////////////////////////////////////////////////////////////////////////////////////////////
    RES_libraries=RES_libraries.filter(_.booksToSend.size > 0)
    // OUTPUT CREATION
    var RES_num_libraries : Int = RES_libraries.length

    val writer = new PrintWriter(new File(path + "output_b.txt"))

    writer.append(RES_num_libraries.toString +"\n") // RES_libraries.length???
    RES_libraries.foreach( library => {
      writer.append(library.ID + " " + library.booksToSend.size +"\n")
      library.booksToSend.foreach(book => writer.append(book + " "))
      writer.append("\n")

    })

    writer.close

  }



}

class Library(var ID : Int, var numbooks : Int, var books : List[Book], var signupdays : Int, var booksxday : Int, var booksToSend : ArrayBuffer[Int]){
  override def toString: String = ID.toString + " :" + numbooks + " " + signupdays + " " + booksxday
}



class Item(var param1 : Int, var param2 : Int) extends Ordered[Item]{
  override def toString: String = param1 + " " + param2

  def compare(that: Item): Int = this.param1.compareTo(that.param1)
}

class Book(var id: Int, scores: List[Int]) extends Ordered[Book]{
  def compare(that: Book): Int = scores.apply(id).compareTo(scores.apply(that.id))
}
