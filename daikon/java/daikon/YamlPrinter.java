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


class YamlMap extends LinkedHashMap<Object,Object> {}
class YamlList extends LinkedList<Object> {}
class Attributes extends LinkedHashMap<String,Object> {}

class JavaObject extends LinkedHashMap<String,Object> {

  private static Set<Object> objectSet = new HashSet<Object>();
  private static List<Object> objectList = new LinkedList<Object>();

  public static boolean isInteresting(Class c){
    return c.isPrimitive()
      || c.getName() == "java.lang.Boolean"
     // || c.getName() == "java.lang.Byte"
      || c.getName() == "java.lang.Character"
      || c.getName() == "java.lang.Double"
      || c.getName() == "java.lang.Float"
      || c.getName() == "java.lang.Integer"
      || c.getName() == "java.lang.Long"
      || c.getName() == "java.lang.Short"
      || c.getName() == "java.lang.String"
      ;
  }

  public static Object simplify(Object x) {
    if(x==null) return "0"; // Assumed null===0, no objects have hashCode 0
    if(isInteresting(x.getClass())) return x; 
    else {
      if(!objectSet.contains(x)){
        objectSet.add(x);
        objectList.add(x);
      }
      return x.hashCode();
    }
  }

  // Get all (including inherited) fields (except from java.lang.Object)
  public static List<Field> getAllFields(Class c){
    List<Field> list = new LinkedList<Field>();

    if(c.getName()=="java.lang.Object") return list;
    else list.addAll(getAllFields(c.getSuperclass()));
    for(Field f: c.getDeclaredFields()) list.add(f);

    return list;
  }

  public static JavaObject getInstance(Object obj){
    JavaObject jobj = new JavaObject();

    jobj.put("@CLASS",obj.getClass().getName());

    if(obj instanceof Map){
      Map map = (Map)obj;
      YamlMap mapping = new YamlMap();

      for(Object key: map.keySet()){
        Object val = map.get(key);
        mapping.put(simplify(key),simplify(val));
      }
      jobj.put("@MAPPING",mapping);
    }
    else if(obj instanceof Collection || obj.getClass().isArray()){
      YamlList list = new YamlList();
      if(obj instanceof Collection) obj = ((Collection)obj).toArray();
      int len = Array.getLength(obj);
      for(int i=0;i<len;i++){
        Object x = Array.get(obj,i);
        list.add(simplify(x));
      }
      jobj.put("@SEQUENCE",list);
    }
    else {
      Attributes attributes = new Attributes();
      for(Field f: getAllFields(obj.getClass())){
        try{
          f.setAccessible(true);
          Object fobj = f.get(obj);
          attributes.put(f.getName(),simplify(fobj));
        }catch(Exception e){
          attributes.put(f.getName(),e.toString());
        }
      }
      jobj.put("@SCALAR",attributes);
    }
    return jobj;
  }

  public static Map<Integer,JavaObject> getInstances(Object obj){
    objectSet = new HashSet<Object>();
    objectList = new LinkedList<Object>();
    Map<Integer,JavaObject> map = new LinkedHashMap<Integer,JavaObject>();

    objectSet.add(obj);
    objectList.add(obj);

    while(objectList.size()>0){
      Object x = objectList.remove(0);
      map.put(x.hashCode(),getInstance(x));
    }

    return map;
  }


}
public class YamlPrinter{

  public static void printInvariants(PptMap all_ppts){
    try{
      Yaml.dump(JavaObject.getInstances(all_ppts), new java.io.File("pptmap.yml"),false);
    }catch(java.io.FileNotFoundException e){}
  }

}

