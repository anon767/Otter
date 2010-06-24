//
// Generated by JTB 1.1.2
//

package jtb.cparser.syntaxtree;

/**
 * Grammar production:
 * f0 -> ParameterList()
 * f1 -> [ "," "..." ]
 */
public class ParameterTypeList implements Node {
  static final long serialVersionUID = 20050923L;

   public ParameterList f0;
   public NodeOptional f1;

   public ParameterTypeList(ParameterList n0, NodeOptional n1) {
      f0 = n0;
      f1 = n1;
   }

   public void accept(jtb.cparser.visitor.Visitor v) {
      v.visit(this);
   }
}