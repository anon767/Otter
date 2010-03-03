/**
 * Export PptMap in YAML format
 *
 *   Schema
 *   [object ID]:
 *     @CLASS: [java class name]
 *     @TYPE: {@SCALAR,@SEQUENCE,@MAPPING}
 *     @CONTENT:
 *        [SCALAR: mapping from field:String to value:(primitive or object ID)]
 *        [SEQUENCE: list of object IDs]
 *        [MAPPING: mapping from object IDs to object IDs]
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

  private static Map<Object,Integer> objectTable = new HashMap<Object,Integer>();
  private static List<Object> objectList = new LinkedList<Object>();

  public static boolean isGenerallyPrimitive(Class c){
    return c.isPrimitive()
      || c.getName() == "java.lang.Boolean"
     // || c.getName() == "java.lang.Byte" // A bug in JYaml that forbids this
      || c.getName() == "java.lang.Character"
      || c.getName() == "java.lang.Double"
      || c.getName() == "java.lang.Float"
      || c.getName() == "java.lang.Integer"
      || c.getName() == "java.lang.Long"
      || c.getName() == "java.lang.Short"
      || c.getName() == "java.lang.String"
      ;
  }

  // 0 is null
  public static int nextNumber_n = 1;
  public static int nextNumber(){
    return nextNumber_n++;
  }
  public static String hash(Object c){
    if(c==null) return "0";
    if(!objectTable.containsKey(c)) return "-1";
    return objectTable.get(c).toString();
  }

  public static Object hashadd(Object x) {
    if(x==null) return hash(x);
    if(isGenerallyPrimitive(x.getClass())) return x.toString(); 
    else {
      if(!objectTable.containsKey(x)){
        objectTable.put(x,nextNumber());
        objectList.add(x);
      }
      return hash(x);
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

    String type = null;
    Object content = null;

    if(obj instanceof Map){
      Map map = (Map)obj;
      YamlMap mapping = new YamlMap();

      for(Object key: map.keySet()){
        Object val = map.get(key);
        mapping.put(hashadd(key),hashadd(val));
      }
      type = "@MAPPING";
      content = mapping;
    }
    else if(obj instanceof Collection){
      YamlList list = new YamlList();
      Collection c = (Collection)obj;
      for(Object x: c){
        // Funny. x!=c, but they have the same hash code
        list.add(hashadd(x));
      }
      type = "@SEQUENCE";
      content = list;
    }
    else if(obj.getClass().isArray()){
      YamlList list = new YamlList();
      int len = Array.getLength(obj); // use Array because obj may be an array of primitive type
      for(int i=0;i<len;i++){
        Object x = Array.get(obj,i);
        list.add(hashadd(x));
      }
      type = "@SEQUENCE";
      content = list;
    }
    else {
      Attributes attributes = new Attributes();
      for(Field f: getAllFields(obj.getClass())){
        try{
          f.setAccessible(true);
          Object fobj = f.get(obj);
          attributes.put(f.getName(),hashadd(fobj));
        }catch(Exception e){
          attributes.put(f.getName(),e.toString());
        }
      }
      type = "@SCALAR";
      content = attributes;
    }

    jobj.put("@CLASS",obj.getClass().getName());
    jobj.put("@TYPE",type);
    jobj.put("@CONTENT",content);

    return jobj;
  }

  public static Map<String,JavaObject> getInstances(Object obj){
    objectTable = new HashMap<Object,Integer>();
    objectList = new LinkedList<Object>();
    Map<String,JavaObject> map = new LinkedHashMap<String,JavaObject>();

    objectTable.put(obj,nextNumber());
    objectList.add(obj);

    while(objectList.size()>0){
      Object x = objectList.remove(0);
      map.put(hash(x),getInstance(x));
    }
    return map;
  }
}

public class YamlPrinter{

  public static void printInvariants(PptMap all_ppts){
    try{
      Yaml.dump(JavaObject.getInstances(all_ppts), new java.io.File("pptmap.yml"),true);
    }catch(java.io.FileNotFoundException e){}
  }

}

