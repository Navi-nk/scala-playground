package week2

import java.util.Random


class QuickSort(isRandomPivot: Boolean) {
   def quicksort(inputArr: Array[Int], low: Int, high: Int): Unit = {
    if (low < high) {
      val p = partition(inputArr, low, high)
      quicksort(inputArr, low, p - 1)
      quicksort(inputArr, p + 1, high)
    }
  }

  private def partition(inputArr: Array[Int], low: Int, high: Int) = {
    if (isRandomPivot) {
      val pivot = new Random().ints(low, high + 1).limit(1).findFirst.getAsInt
      val temp = inputArr(high)
      inputArr(high) = inputArr(pivot)
      inputArr(pivot) = temp
    }
    var i = low
    val pivot = high
    var j = low
    while ( {
      j < high
    }) {
      if (inputArr(j) < inputArr(pivot)) {
        if (i != j) {
          val temp = inputArr(i)
          inputArr(i) = inputArr(j)
          inputArr(j) = temp
        }
        i += 1
      }

      {
        j += 1; j - 1
      }
    }
    val temp = inputArr(pivot)
    inputArr(pivot) = inputArr(i)
    inputArr(i) = temp
    i
  }
}

object QuickSort {
  def apply(isRandomPivot: Boolean): QuickSort = new QuickSort(isRandomPivot)

  def apply(): QuickSort = new QuickSort(false)
}
