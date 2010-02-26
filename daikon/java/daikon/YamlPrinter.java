/**
 * Export PptMap in YAML format
 *
 *
 * Schema:
 *
 * [name]
 *     -
 *       InvType : [type, in Daikon's java class name]
 *       InvData :
 *         [One mapping per field/method call]
 *   
 *
 * @author  Martin
 */

package daikon;

import daikon.inv.Equality;
import daikon.inv.Invariant;
import daikon.inv.OutputFormat;
import daikon.inv.binary.sequenceScalar.*;
import daikon.inv.binary.sequenceString.MemberString;
import daikon.inv.binary.twoScalar.*;
import daikon.inv.binary.twoString.*;
import daikon.inv.binary.twoSequence.*;
import daikon.inv.binary.twoString.*;
import daikon.inv.ternary.threeScalar.*;
import daikon.inv.unary.scalar.*;
import daikon.inv.unary.sequence.*;
import daikon.inv.unary.string.*;
import daikon.inv.unary.stringsequence.*;

import org.ho.yaml.Yaml;
import java.util.*;
import java.lang.reflect.*;


public class YamlPrinter{

  public static List convertObject(Object obj){
    List list = new LinkedList();
    if (obj==null){
      list.add(null);
    }
    else if (obj.getClass().isArray()){
      int len = Array.getLength(obj);
      for(int i=0;i<len;i++)
        list.addAll(convertObject(Array.get(obj,i)));
    }else {
      list.add(obj.toString());
    }
    return list;
  }

  private static boolean interestingReturnType(Class c){
    if (c.isArray()) return interestingReturnType(c.getComponentType());
    return c.isPrimitive() || c.getName()=="java.lang.String";
  }

  public static Map convertInvariant(Invariant inv){
    Class c = inv.getClass();

    Map map = new LinkedHashMap();

    Field[] f = c.getFields();
    for(int i=0;i<f.length;i++)
      try{
        map.put(f[i].getName(),convertObject(f[i].get(inv)));
      }catch(java.lang.IllegalAccessException e){
        System.out.println(e);
      }

    // I assumed that any method with zero arguments and primitive/array return types
    // are getter methods and have no side effects.
    Method[] m = c.getMethods();
    for(int i=0;i<m.length;i++)
      if(m[i].getParameterTypes().length==0 
          &&  interestingReturnType(m[i].getReturnType()))
        try{
          Object ret = m[i].invoke(inv,null);
          if (ret!=null) map.put(m[i].getName(),convertObject(ret));
        }catch(java.lang.IllegalAccessException e){
          System.out.println(e);
        }catch(java.lang.reflect.InvocationTargetException e){
          System.out.println(e);
        }

    Map top = new LinkedHashMap();
    top.put("InvType",c.getName());
    top.put("InvData",map);

    return top;
  }

  public static List convertPptTopLevel(PptTopLevel ppt){
    List list = new LinkedList();
    for (Iterator<Invariant> itor = ppt.getInvariants().iterator(); itor.hasNext();){
      Invariant inv = itor.next();
      Map map = convertInvariant(inv);
      list.add(map);
    }
    return list;
  }

  public static Map convertPptMap(PptMap pptmap){
    Map map = new LinkedHashMap();
    for (Iterator<PptTopLevel> itor = pptmap.ppt_all_iterator(); itor.hasNext();){
        PptTopLevel ppt = itor.next();
        // Only export invariants for function entries
        if (ppt.is_enter()){
          map.put(ppt.ppt_name.getMethodName(),convertPptTopLevel(ppt));
        }
    }
    return map;
  }
  
  public static void printInvariants(PptMap all_ppts){
    Map map = convertPptMap(all_ppts);
    try{
      Yaml.dump(map, new java.io.File("pptmap.yml"),true);
    }catch(java.io.FileNotFoundException e){}
  }

}

