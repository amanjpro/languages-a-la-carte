// package ch.usi.inf.l3.sana.modulej.eval
//
//
// import ch.usi.inf.l3.sana
// import sana.ooj
// import sana.modulej
// import sana.tiny
//
// import tiny.core.TransformationComponent
// import tiny.dsl._
//
//
// import modulej.ast._
// import ooj.ast.PackageDefApi
// import ooj.eval.ConstantCollectingComponent
//
//
//
// @component(tree, env)
// trait CompilationUnitConstantCollectingComponent extends
//     ConstantCollectingComponent {
//   (cunit: CompilationUnitApi)  => {
//     val (module, newEnv) = collect((cunit.module, env))
//     (TreeCopiers.copyCompilationUnit(cunit)(module =
//         module.asInstanceOf[PackageDefApi]), newEnv)
//   }
// }
