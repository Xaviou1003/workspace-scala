package recfun


object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if(c == 0)1
    else if(c > r)0
    else (pascal(c-1, r-1) + pascal(c, r-1))
  }

  /**
   * Exercise 2
   */


  def balance(chars: List[Char]): Boolean = {
    def balanceInternal(chars: List[Char], count: Int): Int ={
      if(chars.size == 0)count
      else {
        val first = chars.head
        if(first == '(') balanceInternal(chars.tail, count + 1)
        else if(first == ')'){
          if(count > 0) balanceInternal(chars.tail, count - 1)
          else -1
        }
        else balanceInternal(chars.tail, count)
      }
    }
    balanceInternal(chars, 0) == 0
  }




  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    countChangeInternal(money, coins)
  }

  def countChangeInternal(money: Int, coins: List[Int]): Int = {
    def calculMoney(money: Int, coins: List[Int]): Int = {
      var result = 0
      if (money == 0 || coins.isEmpty)result
      else {
        val first = coins.head
        var nbCoin = money / first
        while (nbCoin > 0) {
          val rest = money - (nbCoin * first)
          if(rest == 0)result += 1
          else result += countChangeInternal(rest, coins.tail)
          nbCoin -= 1
        }
      }
      result
    }

    var result = calculMoney(money, coins)
    if(coins.size > 1){
      result += countChangeInternal(money, coins.tail)
    }
    result
  }






}
