package com.codenjoy.dojo.bomberman.client

import scala.reflect.ClassTag

object Utils {
  def deepCopy3d[T <: AnyVal : ClassTag](array: Array[Array[Array[T]]]): Array[Array[Array[T]]] = {
    val target = array.clone
    target.map(deepCopy2d).toArray
  }

  def deepCopy2d[T <: AnyVal : ClassTag](array: Array[Array[T]]): Array[Array[T]] =  array.clone.map(_.clone())
}
