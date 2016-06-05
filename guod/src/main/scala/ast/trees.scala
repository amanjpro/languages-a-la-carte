/*
 * Copyright (c) <2015-2016>, see CONTRIBUTERS
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the <organization> nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package ch.usi.inf.l3.sana.guod

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import sana.arrayj
import sana.ooj
import sana.robustj
import sana.ppj
import sana.dynj
import sana.modulej


package object ast {
  // Trees
  type Tree                 = tiny.ast.Tree
  type Expr                 = tiny.ast.Expr
  type UseTree              = tiny.ast.UseTree
  type DefTree              = tiny.ast.DefTree
  type TypeTree             = tiny.ast.TypeTree
  type TermTree             = tiny.ast.TermTree
  type NamedTree            = tiny.ast.NamedTree
  type SimpleUseTree        = tiny.ast.SimpleUseTree
  val NoTree                = tiny.ast.NoTree

  type IdentApi             = tiny.ast.IdentApi
  type TypeUseApi           = tiny.ast.TypeUseApi


  type CastApi              = calcj.ast.CastApi
  type LiteralApi           = calcj.ast.LiteralApi
  type UnaryApi             = calcj.ast.UnaryApi
  type BinaryApi            = calcj.ast.BinaryApi


  type BlockApi             = primj.ast.BlockApi
  type WhileApi             = primj.ast.WhileApi
  type IfApi                = primj.ast.IfApi
  type ForApi               = primj.ast.ForApi
  type AssignApi            = primj.ast.AssignApi
  type ApplyApi             = primj.ast.ApplyApi
  type ValDefApi            = primj.ast.ValDefApi
  type TernaryApi           = primj.ast.TernaryApi
  type ReturnApi            = primj.ast.ReturnApi


  type LabelApi             = brokenj.ast.LabelApi
  type BreakApi             = brokenj.ast.BreakApi
  type ContinueApi          = brokenj.ast.ContinueApi
  type CaseApi              = brokenj.ast.CaseApi
  type SwitchApi            = brokenj.ast.SwitchApi

  type ArrayInitializerApi  = arrayj.ast.ArrayInitializerApi
  type ArrayTypeUseApi      = arrayj.ast.ArrayTypeUseApi
  type ArrayAccessApi       = arrayj.ast.ArrayAccessApi
  type ArrayCreationApi     = arrayj.ast.ArrayCreationApi


  type ProgramApi           = ooj.ast.ProgramApi
  type PackageDefApi        = ooj.ast.PackageDefApi
  type ClassDefApi          = ooj.ast.ClassDefApi
  type ThisApi              = ooj.ast.ThisApi
  type SuperApi             = ooj.ast.SuperApi
  type TemplateApi          = ooj.ast.TemplateApi
  type NewApi               = ooj.ast.NewApi
  type SelectApi            = ooj.ast.SelectApi

  type TryApi               = robustj.ast.TryApi
  type MethodDefApi         = robustj.ast.MethodDefApi
  type CatchApi             = robustj.ast.CatchApi
  type ThrowApi             = robustj.ast.ThrowApi

  type SynchronizedApi      = ppj.ast.SynchronizedApi

  type CompilationUnitApi   = modulej.ast.CompilationUnitApi


  // Constants

  type ByteConstant          = calcj.ast.ByteConstant
  type ShortConstant         = calcj.ast.ShortConstant
  type CharConstant          = calcj.ast.CharConstant
  type IntConstant           = calcj.ast.IntConstant
  type LongConstant          = calcj.ast.LongConstant
  type FloatConstant         = calcj.ast.FloatConstant
  type DoubleConstant        = calcj.ast.DoubleConstant
  type BooleanConstant       = calcj.ast.BooleanConstant
  type StringConstant        = ooj.ast.StringConstant
  val ByteConstant           = calcj.ast.ByteConstant
  val ShortConstant          = calcj.ast.ShortConstant
  val CharConstant           = calcj.ast.CharConstant
  val IntConstant            = calcj.ast.IntConstant
  val LongConstant           = calcj.ast.LongConstant
  val FloatConstant          = calcj.ast.FloatConstant
  val DoubleConstant         = calcj.ast.DoubleConstant
  val BooleanConstant        = calcj.ast.BooleanConstant
  val StringConstant         = ooj.ast.StringConstant
  val NullConstant           = ooj.ast.NullConstant

  // Factories, Copiers and Extractors

  val TreeExtractors        = modulej.ast.TreeExtractors
  val TreeCopiers           = modulej.ast.TreeCopiers
  val TreeFactories         = modulej.ast.TreeFactories
}
