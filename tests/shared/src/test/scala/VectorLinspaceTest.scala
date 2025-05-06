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

import slash.matrix.*
import slash.vector.*

class VectorLinspaceTest extends munit.FunSuite {

  test("linspace produces expected result"){
    val v = Vec.linspace[5](-1, +1)
    val expect = Vec[5](-1.0, -0.5, 0.0, 0.5, 1.0)
    printf("%s\n",v.show)
    printf("%s\n",expect.show)
    assert(v.asRowMatrix.strictEquals(expect.asRowMatrix))
  }

}
