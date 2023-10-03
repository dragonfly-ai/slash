/*
 * Copyright 2023 dragonfly.ai
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ai.dragonfly.math.stats.probability.distributions

import narr.*

import scala.reflect.ClassTag


trait Sampleable[DOMAIN:ClassTag] {
  def random(r:scala.util.Random = ai.dragonfly.math.Random.defaultRandom): DOMAIN

  inline def sample(n: Int, r:scala.util.Random = ai.dragonfly.math.Random.defaultRandom): NArray[DOMAIN] = NArray.tabulate(n)(_ => random(r))

}
