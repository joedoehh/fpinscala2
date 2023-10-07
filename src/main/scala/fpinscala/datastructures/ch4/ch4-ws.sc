val ls = List.apply(1.0, 2.0, 3.0)

def mean(xs: Seq[Double]): Option[Double] =
  if xs.isEmpty then None
  else Some(xs.sum / xs.length)

val m = mean(ls).get

ls.map(x => math.pow(x-m,2))

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

val v = variance(ls)



