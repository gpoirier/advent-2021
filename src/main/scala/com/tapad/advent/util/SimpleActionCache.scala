package com.tapad.advent.util

import cats.implicits._
import cats.effect.implicits._
import cats.effect._

import scala.language.higherKinds

case class SimpleActionCache[F[_]: Concurrent, K, V](ref: Ref[F, Map[K, F[V]]]) {
  def get(key: K)(action: F[V]): F[V] = {
    ref.get.flatMap { map =>
      map.getOrElse(
        key,
        action.memoize flatMap { newAction =>
          val action = ref.modify { map =>
            map.get(key) match {
              case Some(value) => map -> value
              case None =>
                map.updated(key, newAction) -> newAction
            }
          }
          action.flatten
        }
      )
    }
  }
}

object SimpleActionCache {
  def make[F[_]: Async, K, V]: F[SimpleActionCache[F, K, V]] =
    Ref.of(Map.empty[K, F[V]]).map(SimpleActionCache(_))
}
