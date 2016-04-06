object equationSolver {
  def roots(coef: Array[Double]): List[Double] = {
    def sign(value: Double): Int = if (value < 0)-1 else if (value > 0) 1 else 0
    
    // quadratic solution
		def quadr(quad: Array[Double]): List[Double] = {
				val a: Double = quad(1)
	      val b: Double = quad(2)
	      val c: Double = quad(3)
	      val D: Double = b * b - 4 * a * c
	      
	      if (D >= 0) {
	        List((-b + math.sqrt(D)) / (2 * a), (-b - math.sqrt(D)) / (2 * a))
	      }
	      else {
	        List()
	      }
	  }
	  
	  // cube solution
		def cube(cub: Array[Double]): List[Double] = {
	    val i: Double = cub(0)
	    val a: Double = cub(1) / i
	    val b: Double = cub(2) / i
	    val c: Double = cub(3) / i
	    val q: Double = (a * a - 3 * b) / 9
	    val r: Double = (2 * a * a * a - 9 * a * b + 27 * c) / 54
	    if (r * r < q * q * q) {
	      val t: Double = math.acos(r / math.sqrt(q * q * q)) / 3
	      List(-2 * math.sqrt(q) * math.cos(t) - a / 3,
	        -2 * math.sqrt(q) * math.cos(t - 2 * math.Pi / 3) - a / 3,
	        -2 * math.sqrt(q) * math.cos(t + 2 * math.Pi / 3) - a / 3)
	    }
	    else {
	      val m: Double = -sign(r) * math.pow(math.abs(r) + math.sqrt(r * r - q * q * q), 1 / 3)
	      var n: Double = 0
	      if (m != 0) {
	        n = q / m
	      }
	      if (m == n) {
	        List((m + n) - a / 3, -m - a / 3)
	      }
	      else {
	        List((m + n) - a / 3)
	      }
	    }
		}
		
    if (coef(0) == 0) {
      quadr(coef)
    }
    else {
      cube(coef)
    }
  }                                               
}
