(*
 *
 * Copyright (c) 2012-2014, 
 *  Wes Weimer          <weimer@cs.virginia.edu>
 *  Stephanie Forrest   <forrest@cs.unm.edu>
 *  Claire Le Goues     <legoues@cs.virginia.edu>
 *  Jonathan Dorn       <dorn@virginia.edu>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)

(**
   Self-contained Pseudo-Random Number Generators.

   Since the Ocaml standard library's PRNG uses global state, it is not thread
   safe. The objects in this module each maintain their own separate state,
   allowing them to be used safely to (re)generate a sequence of random numbers
   in a thread safe manner.

   Much of this implementation is a straight-forward port from
   {{:http://commons.apache.org/proper/commons-math/javadocs/api-3.2/org/apache/commons/math3/random/package-summary.html}
   Apache Commons Math3}.
 *)

(**
   Interface for a PRNG. This file contains at least one concrete implementation
   of this interface.
 *)
class type gen = object
  (** Sets the seed of the underlying random number generator. *)
  method setSeed : Int32.t list -> gen

  (** Returns a pseudo-random, uniformly distributed int value. *)
  method nextInt : unit -> int

  (** Returns a pseudo-random, uniformly distributed int32 value. *)
  method nextInt32 : unit -> Int32.t

  (** Returns a pseudo-random float, uniformly distributed between 0.0 and
      1.0. *)
  method nextFloat : unit -> float
end

(**
   Implements the
   {{:http://www.iro.umontreal.ca/~lecuyer/myftp/papers/lfsr04.pdf} Well512}
   pseudo-random number generator.
 *)
(* TODO: much of this class should be refactored into a common abstract class
   so that other PRNGs can reuse the implementation. *)
class well512 =
  let r = 16 in
  let m1 = 13 in
  let m2 =  9 in
  let b = Int32.logor
            (Int32.shift_left (Int32.of_int 0xda44) 16)
            (Int32.of_int 0x2d24) in
  object (self)
    (**/**)

    (** The PRNG state. *)
    val state = Array.make r Int32.zero

    (** Points to the element of the state that is currently in use. *)
    val mutable index = 0
    (**/**)

    (** Sets the seed of the underlying random number generator. If the seed is
        shorter than the internal state of this PRNG, it will be extended with
        data based on what is provided. *)
    method setSeed seed =
      let length =
        List.fold_left (fun j i -> 
          if j < r then
            state.(j) <- i;
          j + 1
        ) 0 seed
      in
        for j = length to (r-1) do
          let i = Int64.of_int32 state.(j - length) in
          state.(j) <- Int64.to_int32 (
            Int64.add
              (Int64.mul
                (Int64.of_int 1812433253)
                (Int64.logxor i (Int64.shift_right i 30)))
              (Int64.of_int j))
        done;
        index <- 0;
        self

    (**/**)

    (** Computes the next uniformly distributed 32 bits from this PRNG and
        updates the state. *)
    method private next bits =
      let v0   = state.(index) in
      let vm1  = state.((index + m1) land 0xf) in
      let vm2  = state.((index + m2) land 0xf) in

      let z0 = state.((index + 15) land 0xf) in
      let z1 = Int32.logxor
                (Int32.logxor v0 (Int32.shift_left v0 16))
                (Int32.logxor vm1 (Int32.shift_left vm1 15)) in
      let z2 = Int32.logxor vm2 (Int32.shift_right vm2 11) in
      let newV1 = Int32.logxor z1 z2 in
      let newV0 = Int32.logxor
                    (Int32.logxor
                      (Int32.logxor z0 (Int32.shift_left z0 2))
                      (Int32.logxor z1 (Int32.shift_left z1 18)))
                    (Int32.logxor
                      (Int32.shift_left z2 28)
                      (Int32.logxor
                        newV1
                        (Int32.logand (Int32.shift_left newV1 5) b))) in
      state.(index) <- newV1;
      state.((index + 15) land 0xf) <- newV0;
      index <- (index + 15) land 0xf;
      Int32.shift_right_logical state.(index) (32 - bits)
    (**/**)

    (** Returns a pseudo-random, uniformly distributed int32 value. *)
    method nextInt32 () = self#next 32

    (** Returns a pseudo-random, uniformly distributed int value. *)
    method nextInt () =
      Int32.to_int (self#next 31)

    (** Returns a pseudo-random float, uniformly distributed between 0.0 and
        1.0 *)
    method nextFloat () =
      let high = Int64.shift_left (Int64.of_int32 (self#next 26)) 26 in
      let low  = Int64.of_int32 (self#next 26) in
      (Int64.to_float (Int64.logor high low)) *. epsilon_float
end

