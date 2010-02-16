// ***** This file is automatically generated from Member.java.jpp

package daikon.inv.binary.sequenceScalar;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.twoSequence.*;
import daikon.derive.unary.*;
import daikon.derive.binary.*;
import daikon.derive.ternary.*;
import utilMDE.*;
import java.util.logging.Logger;
import java.util.logging.Level;

/**
 * Represents double scalars that are always members of a sequence of double
 * values.
 * Prints as <samp>x in y[]</samp> where <samp>x</samp> is a double scalar
 * and <samp>y[]</samp> is a sequence of double.
 **/
public final class MemberFloat
  extends SequenceFloat
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20031024L;

  public static final Logger debug =
    Logger.getLogger ("daikon.inv.binary.Member");

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /**
   * Boolean.  True iff Member invariants should be considered.
   **/
  public static boolean dkconfig_enabled = true;

  protected MemberFloat(PptSlice ppt) {
    super(ppt);
  }

  protected /*@Prototype*/ MemberFloat() {
    super();
  }

  private static /*@Prototype*/ MemberFloat proto;

  /** Returns the prototype invariant for MemberFloat **/
  public static /*@Prototype*/ MemberFloat get_proto() {
    if (proto == null)
      proto = new /*@Prototype*/ MemberFloat ();
    return (proto);
  }

  /** Returns whether or not this invariant is enabled **/
  public boolean enabled() {
    return dkconfig_enabled;
  }

  /** instantiates the invariant on the specified slice **/
  protected MemberFloat instantiate_dyn (/*@Prototype*/ PptSlice slice) {
    return new MemberFloat (slice);
  }

  public /*@Nullable*/ DiscardInfo isObviousStatically(VarInfo[] vis) {
    if (isObviousMember(sclvar(vis), seqvar(vis))) {
      log ("scalar is obvious member of");
      return new DiscardInfo(this, DiscardCode.obvious, sclvar().name()
                        + " is an obvious member of " + seqvar().name());
    }
    return super.isObviousStatically (vis);
  }

  /**
   * Check whether sclvar is a member of seqvar can be determined
   * statically.
   **/
  public static boolean isObviousMember(VarInfo sclvar, VarInfo seqvar) {

    VarInfo sclvar_seq = sclvar.isDerivedSequenceMember();

    if (sclvar_seq == null) {
      // The scalar is not obviously (lexically) a member of any array.
      return false;
    }
    // isObviousImplied: a[i] in a; max(a) in a
    if (sclvar_seq == seqvar) {
      // The scalar is a member of the same array.
      return true;
    }
    // The scalar is a member of a different array than the sequence.
    // But maybe the relationship is still obvious, so keep checking.

    // isObviousImplied: when b==a[0..i]:  b[j] in a; max(b) in a
    // If the scalar is a member of a subsequence of the sequence, then
    // the scalar is a member of the full sequence.
    // This is satisfied, for instance, when determining that
    // max(B[0..I]) is an obvious member of B.
    VarInfo sclseqsuper = sclvar_seq.isDerivedSubSequenceOf();
    if (sclseqsuper == seqvar)
      return true;

    // We know the scalar was derived from some array, but not from the
    // sequence variable.  If also not from what the sequence variable was
    // derived from, we don't know anything about membership.
    // Check:
    //  * whether comparing B[I] to B[0..J]
    //  * whether comparing min(B[0..I]) to B[0..J]
    VarInfo seqvar_super = seqvar.isDerivedSubSequenceOf();
    if ((seqvar_super != sclvar_seq)
        && (seqvar_super != sclseqsuper)) {
      return false;
    }

    // If the scalar is a positional element of the sequence from which
    // the sequence at hand was derived, then any relationship will be
    // (mostly) obvious by comparing the length of the sequence to the
    // index.  By contrast, if the scalar is max(...) or min(...), all bets
    // are off.
    if (seqvar.derived instanceof SequenceFloatSubsequence ||
        seqvar.derived instanceof SequenceFloatArbitrarySubsequence) {

      // Determine the left index/shift and right index/shift of the
      // subsequence.  If the left VarInfo is null, the sequence starts
      // at the beginning.  If the right VarInfo is null, the sequence stops
      // at the end.
      VarInfo seq_left_index = null, seq_right_index = null;
      int seq_left_shift = 0, seq_right_shift = 0;
      if (seqvar.derived instanceof SequenceFloatSubsequence) {
        // the sequence is B[0..J-1] or similar.  Get information about it.
        SequenceFloatSubsequence seqsss
          = (SequenceFloatSubsequence)seqvar.derived;
        if (seqsss.from_start) {
          seq_right_index = seqsss.sclvar();
          seq_right_shift = seqsss.index_shift;
        } else {
          seq_left_index = seqsss.sclvar();
          seq_left_shift = seqsss.index_shift;
        }
      } else if (seqvar.derived instanceof SequenceFloatArbitrarySubsequence) {
        // the sequence is B[I+1..J] or similar
        SequenceFloatArbitrarySubsequence ssass = (SequenceFloatArbitrarySubsequence)seqvar.derived;
        seq_left_index = ssass.startvar();
        seq_left_shift = (ssass.left_closed ? 0 : 1);
        seq_right_index = ssass.endvar();
        seq_right_shift = (ssass.right_closed ? 0 : -1);
      } else {
        throw new Error();
      }

      // if the scalar is a a subscript (b[i])
      if (sclvar.derived instanceof SequenceFloatSubscript) {

        SequenceFloatSubscript sclsss = (SequenceFloatSubscript) sclvar.derived;
        VarInfo scl_index = sclsss.sclvar(); // "I" in "B[I]"
        int scl_shift = sclsss.index_shift;

        // determine if the scalar index is statically included in the
        // left/right sequence
        boolean left_included = false, right_included = false;
        if (seq_left_index == null)
          left_included = true;
        if (seq_left_index == scl_index) {
          if (seq_left_shift <= scl_shift) left_included = true;
        }
        if (seq_right_index == null)
          right_included = true;
        if (seq_right_index == scl_index) {
          if (seq_right_shift >= scl_shift) right_included = true;
        }
        if (left_included && right_included)
          return true;

      // else if the scalar is a specific positional element (eg, b[0])
      } else if (sclvar.derived instanceof SequenceInitialFloat) {

        // isObviousImplied: B[0] in B[0..J]; also B[-1] in B[J..]
        SequenceInitialFloat sclse = (SequenceInitialFloat) sclvar.derived;
        int scl_index = sclse.index;
        if (((scl_index == 0) && seq_left_index == null)
            || ((scl_index == -1) && seq_right_index == null))
          // It might not be true, because the array could be empty;
          // but if the array isn't empty, then it's obvious.  The empty
          // array case is ok, because the variable itself would be
          // destroyed in that case.
          return true;

      // else if the scalar is min or max of a sequence
      } else if ((sclvar.derived instanceof SequenceMin)
                 || (sclvar.derived instanceof SequenceMax)) {
        Pair<DiscardCode,String> di = SubSequenceFloat.isObviousSubSequence(sclvar_seq, seqvar);
        return (di != null);
      }
    }

    return false;
  }

  public String repr() {
    return "Member" + varNames() + ": "
      + "falsified=" + falsified;
  }

  public String format_using(OutputFormat format) {

    if (format.isJavaFamily()) return format_java_family(format);

    if (format == OutputFormat.DAIKON) {
      return format_daikon();
    } else if (format == OutputFormat.ESCJAVA) {
      return format_esc();
    } else {
      return format_unimplemented(format);
    }
  }

  public String format_daikon() {
    String sclname = sclvar().name();
    String seqname = seqvar().name();
    return sclname + " in " + seqname;
  }

  public String format_java() {
    return "( (daikon.inv.FormatJavaHelper.memberOf("
      + sclvar().name()
      + " , " + seqvar().name() + " ) == true ) ";
  }

  public String format_java_family(OutputFormat format) {
    String sclname = sclvar().name_using(format);
    String seqname = seqvar().name_using(format);
    return "daikon.Quant.memberOf(" + sclname
      + " , " + seqname + " )";
  }

  public String format_esc() {
    // "exists x in a..b : P(x)" gets written as "!(forall x in a..b : !P(x))"
    String[] form = VarInfo.esc_quantify (seqvar(), sclvar());
    return "!" + form[0] + "(" + form[1] + " != " + form[2] + ")" + form[3];
  }

  public InvariantStatus check_modified(double[] a, double i, int count) {
    if (a == null) {
      return InvariantStatus.FALSIFIED;
    } else if (Global.fuzzy.indexOf(a, i) == -1) {
      return InvariantStatus.FALSIFIED;
    }
    return InvariantStatus.NO_CHANGE;
  }

  public InvariantStatus add_modified(double[] a, double i, int count) {

    InvariantStatus is = check_modified(a, i, count);
    if (debug.isLoggable(Level.FINE) && (is == InvariantStatus.FALSIFIED))
      debug.fine ("Member destroyed:  " + format() + " because " + i +
                  " not in " + ArraysMDE.toString(a));
    return (is);
  }

  protected double computeConfidence() {
    return Invariant.CONFIDENCE_JUSTIFIED;
  }

  public boolean isSameFormula(Invariant other) {
    assert other instanceof MemberFloat;
    return true;
  }

  /**
   * Checks to see if this is obvious over the specified variables.
   * Implements the following checks: <pre>
   *
   *   (0 <= i <= j) ^ (A[] == B[]) ==> A[i] in B[0..j]
   *   (0 <= i <= j) ^ (A[] == B[]) ==> A[j] in B[i..]
   *   (A subset B)                 ==> A[i] in B
   * </pre>
   */
  public /*@Nullable*/ DiscardInfo isObviousDynamically (VarInfo[] vis) {

    if (Debug.logOn())
      Debug.log (getClass(), ppt.parent, vis, "is obvious check" + format());

    DiscardInfo super_result = super.isObviousDynamically(vis);
    if (super_result != null) {
      return super_result;
    }

    // Check for subscript in subsequence
    DiscardInfo di = subscript_in_subsequence (vis);
    if (di != null)
      return (di);

    // Check for (A subset B) ==> (A[i] member B)
    di = subset_in_subsequence (vis);
    if (di != null)
      return (di);

    return (null);
  }

  /*
   * Checks to see if this is obvious over the specified variables.
   * Implements the following check: <pre>
   *
   * (A subset B) ==> (A[i] member B)
   * </pre>
   */
  private /*@Nullable*/ DiscardInfo subset_in_subsequence (VarInfo[] vis) {

    VarInfo scl_var = sclvar (vis);
    VarInfo seq_var = seqvar (vis);

    if (Debug.logOn())
      Debug.log (getClass(), ppt.parent, vis, "looking for subset in subseq");

    // If the scalar is not a subscript of a seq there is nothing to check.
    if (scl_var.derived == null)
      return null;
    if (!(scl_var.derived instanceof SequenceFloatSubscript))
      return null;
    SequenceFloatSubscript sssc = (SequenceFloatSubscript) scl_var.derived;
    if (Debug.logOn())
      Debug.log (getClass(), ppt.parent, vis, "Looking for "
                 + sssc.seqvar().name() + " subset of "
                 + seq_var.name());

    // check to see if subscripts sequence is a subset of the sequence var
    if (ppt.parent.is_subset (sssc.seqvar(), seq_var)) {
      return new DiscardInfo(this, DiscardCode.obvious,
                             "(" + sssc.seqvar().name() + " subset "
                             + seq_var.name() + ") ==> "
                             + sssc.seqvar().name() + "["
                             + sssc.sclvar().name()
                             + "] member " + seq_var.name());
    }

    return (null);
  }

  /*
   * Checks to see if this is obvious over the specified variables.
   * Implements the following checks: <pre>
   *
   *   (0 <= i <= j) ^ (a[] == b[]) ==> a[i] in b[0..j]
   *   (0 <= i <= j) ^ (a[] == b[]) ==> a[j] in b[i..]
   * </pre>
   */
  private /*@Nullable*/ DiscardInfo subscript_in_subsequence (VarInfo[] vis) {

    VarInfo scl_var = sclvar (vis);
    VarInfo seq_var = seqvar (vis);

    // Both variables must be derived
    if ((scl_var.derived == null) || (seq_var.derived == null))
      return (null);

    // If the scalar is not SequenceScalarSubscript, there is nothing to check.
    if (!(scl_var.derived instanceof SequenceFloatSubscript))
      return null;
    SequenceFloatSubscript sssc = (SequenceFloatSubscript) scl_var.derived;

    // If the sequence is not SequenceScalarSubsequence, nothing to check
    if (!(seq_var.derived instanceof SequenceFloatSubsequence))
      return (null);
    SequenceFloatSubsequence ssss = (SequenceFloatSubsequence) seq_var.derived;

    // Both variables must be derived from equal sequences
    if (!ppt.parent.is_equal (sssc.seqvar(), ssss.seqvar()))
      return (null);

    // if a[i] in a[0..n], look for i <= n
    if (ssss.from_start) {
      if (Debug.logOn())
        Debug.log (getClass(), ppt.parent, vis,
          "Looking for " + sssc.sclvar().name() + sssc.index_shift
           + " <= " + ssss.sclvar().name() + ssss.index_shift);
      if (ppt.parent.is_less_equal (sssc.sclvar(), sssc.index_shift,
                             ssss.sclvar(), ssss.index_shift))
        return new DiscardInfo(this, DiscardCode.obvious,
                     "i <= n ==> a[i] in a[..n] for "
                     + scl_var.name() + " and " + seq_var.name());
    } else { // a[i] in a[n..], look for i >= n
      if (Debug.logOn())
        Debug.log (getClass(), ppt.parent, vis,
          "Looking for " + ssss.sclvar().name() + ssss.index_shift
           + " <= " + sssc.sclvar().name() + sssc.index_shift);
      if (ppt.parent.is_less_equal (ssss.sclvar(), ssss.index_shift,
                             sssc.sclvar(), sssc.index_shift))
        return new DiscardInfo(this, DiscardCode.obvious,
                     "i >= n ==> a[i] in a[n..] for "
                     + scl_var.name() + " and " + seq_var.name());
    }
    return (null);
  }

}
