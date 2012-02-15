/**
 *Brandon Gier
 *gier.b@husky.neu.edu
 *The program throws an -xlint error, but still performs correctly
 *  given the test program
 */

/*FMap<K,V> is an immutable abstract data type whose values represent
finite functions from keys of type K to values of type V.

The FMap<K,V> ADT shall be implemented in Java, and will be tested
using Sun's Java 2 Runtime Environment, Standard Edition, version 1.6.0.
The code for this implementation shall be in the default package, and
shall define a public class named FMap.  The operations of the
FMap class shall be provided by the following public methods
of the FMap class:

Signature:

  Static methods:

    emptyMap     :                    ->  FMap

  Dynamic methods:

    add          :  K x V             ->  FMap
    isEmpty      :                    ->  boolean
    size         :                    ->  int
    containsKey  :  K                 ->  boolean
    get          :  K                 ->  V
    toString     :                    ->  String
    equals       :  Object            ->  boolean
    hashCode     :                    ->  int

Restrictions:

    Null arguments may not be passed to any of the above methods.

Algebraic specification:

    FMap.emptyMap().isEmpty()  =  true
    m0.add(k0, v0).isEmpty()  =  false

    FMap.emptyMap().size()  =  0
    m0.add(k0, v0).size()
        =  m0.size()                           if m0.containsKey(k0)
    m0.add(k0, v0).size()
        =  1 + m0.size()                       if ! (m0.containsKey(k0))

    FMap.emptyMap().containsKey(x)  =  false
    m0.add(k0, v0).containsKey(x)
        =  true                                if x.equals(k0)
    m0.add(k0, v0).containsKey(x)
        =  m0.containsKey(x)                   if ! x.equals(k0)

    m0.add(k0, v0).get(x)
        =  v0                                  if x.equals(k0)
    m0.add(k0, v0).get(x)
        =  m0.get(x)                           if ! x.equals(k0)

    m.toString() = "{...(" + m.size() + " entries)...}"
*/

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.NoSuchElementException;

public abstract class FMap<K,V> implements Iterable<K>{

    //Contains code to implement dynamic methods from specification
    public abstract FMap<K,V> add(K key, V val);
    public abstract boolean isEmpty();
    public abstract int size();
    public abstract boolean containsKey(K key);
    public abstract V get(K i);
    public abstract FMap<K,V> accept(Visitor<K,V> v);

    public abstract Iterator<K> iterator();

    //Helper methods designed to carry out specification
    abstract FMap<K,V> changed(K ke, V va);
    abstract ArrayList<K> uniquekeys();

    //Static method to create a new emptyMap.
    public static FMap emptyMap(){
        return new Empty();
    }

    //Returns a REdBlack tree implementation of FMap to meet time requirements
    public static FMap emptyMap(Comparator comp){
        return FTree.emptyTree(comp);
    }
}

//Implementation for Red-Black trees
abstract class FTree<K,V> extends FMap<K,V>{
    //Abstract methods from the specification. IMPLEMENTOR ONLY
    abstract FTree<K,V> ins(K k, V v);
    abstract FTree<K,V> makeBlack();
    abstract Color color();
    abstract K key();
    abstract V value();
    abstract FTree<K,V> left();
    abstract FTree<K,V> right();
    abstract int height();

    //Returns a new emptyTree with a comparator
    static FTree emptyTree(Comparator c){
        return new EmptyTree(c);
    }

    //Returns an FTree with a Key and Value added
    static <K,V> FMap<K,V> add(FTree f, K k, V v){
        return f.add(k, v);
    }

    //Returns a new node in a Tree
    static <K,V> FTree<K,V> node(Comparator c, Color col,
        K k, V v, FTree<K,V> l, FTree<K,V> r){
        return new Node(c, col, k, v, l, r);
    }

    //Balances a RB Tree on the left side
    static <K,V> FTree<K, V> balance(Comparator<? super K> c, Color col,
        K k, V v, FTree<K,V> l, FTree<K,V> r){
        if(l.isEmpty())
            FTree.balance2(c, col, k, v, l, r);
        if(col.isBlack() && l.color().isRed() &&
            !(l.left().isEmpty()) && l.left().color().isRed())
            return FTree.node(c, Color.red(), l.key(), l.value(),
                     FTree.node(c, Color.black(), l.left().key(),
                      l.left().value(), l.left().left(), l.left().right()),
                     FTree.node(c, Color.black(), k, v, l.right(), r));
        if(col.isBlack() && l.color().isRed() && !(l.right().isEmpty())
            && l.right().color().isRed())
        {
            return FTree.node(c, Color.red(), l.right().key(),
                     l.right().value(),
                     FTree.node(c,
                         Color.black(), l.key(), l.value(), l.left(),
                         l.right().left()),
                     FTree.node(c, Color.black(),
                        k, v, l.right().right(), r));
        }
        return FTree.balance2(c, col, k, v, l, r);
    }

    //Balances a RB tree on the right side
    static <K,V> FTree<K,V> balance2(Comparator c, Color col,
        K k, V v, FTree l, FTree r){
        if(r.isEmpty())
            return FTree.node(c, col, k, v, l, r);
        if(col.isBlack() && r.color().isRed() && !(r.left().isEmpty())
            && r.left().color().isRed())
            return FTree.node(c, Color.red(), r.left().key(), r.left().value(),
                FTree.node(c, Color.black(), k, v, l, r.left().left()),
                FTree.node(c, Color.black(), r.key(),
                           r.value(), r.left().right(),
                            r.right()));
        if(col.isBlack() && r.color().isRed() &&
            !(r.right().isEmpty()) && r.right().color().isRed())
            return FTree.node(c, Color.red(), r.key(), r.value(),
                              FTree.node(c, Color.black(), k, v, l, r.left()),
                              FTree.node(c, Color.black(), r.right().key(),
                                         r.right().value(), r.right().left(),
                                         r.right().right()));
        return FTree.node(c, col, k, v, l, r);
    }
}

//Represents an empty RB Tree
class EmptyTree<K,V> extends FTree<K,V>{

    //Holds the comparator used
    Comparator<? super K> compare;

    //Basic constructor for a RB tree
    public EmptyTree(Comparator c){
        compare = c;
    }

    //Following methods implement both the FMap and FTree methods
    public boolean isEmpty(){
        return true;
    }

    public FMap<K,V> accept(Visitor<K,V> v){
        return this;
    }

    public int size(){
        return 0;
    }

    public boolean containsKey(K k){
        return false;
    }

    public V get(K x){
        return null;
    }

    FTree<K,V> ins(K k, V v){
        return new Node(compare, Color.red(), k, v,
            FTree.emptyTree(compare), FTree.emptyTree(compare));
    }

    int height(){
        return 0;
    }

    FTree<K,V> right(){
        return this;
    }

    FTree<K,V> left(){
        return this;
    }

    V value(){
        return null;
    }

    K key(){
        return null;
    }

    Color color(){
        return Color.black();
    }

    FTree<K,V> makeBlack(){
        return this;
    }

    ArrayList<K> uniquekeys(){
        return new ArrayList<K>();
    }

    FMap<K,V> changed(K k, V v){
        return this;
    }

    public FTree<K,V> add(K k, V v){
        FTree<K,V> ret = ins(k, v);
        ret = ret.makeBlack();
        return ret;
    }

    public int hashCode(){
        return 0;
    }

    public String toString(){
        return "{...(" + size() + " entries)...}";
    }

    public boolean equals(Object o){
        return (o instanceof FMap && ((FMap)o).isEmpty());
    }

    public Iterator<K> iterator(){
        return new MapIt<K>(this.uniquekeys());
    }
}

//Represents a node in the RB Tree
class Node<K,V> extends FTree<K,V>{

    //Private instance variables used in the class.
    private Comparator<? super K> comp;
    private Color col;
    private K key;
    private V value;
    private FTree<K,V> left;
    private FTree<K,V> right;
    private int size;

    //Constructor for the Node. c is the comparator, col is the Color
    //k is the key here, v is the value, f1 is the left, f2 is the right
    public Node(Comparator<? super K> c, Color col, K k,
                  V v, FTree<K,V> f1, FTree<K,V> f2){
        comp = c;
        this.col = col;
        key = k;
        value = v;
        left = f1;
        right = f2;
        size = 1 + left.size() + right.size();
    }

    //Implementations of the FTree and FMap classes
    int height(){
        return 1+ Math.max(left.height(), right.height());
    }

    FTree<K,V> right(){
        return right;
    }

    FTree<K,V> left(){
        return left;
    }

    V value(){
        return value;
    }

    K key(){
        return key;
    }

    Color color(){
        return col;
    }

    FTree<K,V> makeBlack(){
        return new Node(comp, Color.black(), key, value, left, right);
    }

    FTree<K,V> ins(K k, V v){
        if(comp.compare(k, key) < 0)
            return FTree.balance(comp, col, key, value,
                left.ins(k, v), right);
        if(comp.compare(k, key) == 0)
            return FTree.node(comp, col, key, v, left, right);
        return FTree.balance(comp, col, key, value, left, right.ins(k, v));
    }

    ArrayList<K> uniquekeys(){
        ArrayList<K> keys = new ArrayList<K>();
        keys.addAll(left.uniquekeys());
        keys.add(key);
        keys.addAll(right.uniquekeys());
        return keys;
    }

    FMap<K,V> changed(K k, V v){
        return this;
    }

    public V get(K k){
        if (comp.compare(k, key) < 0)
            return left.get(k);
        if(comp.compare(k, key) == 0)
            return value;
        return right.get(k);
    }

    public boolean containsKey(K k){
        if(comp.compare(k, key) < 0)
            return left.containsKey(k);
        if(comp.compare(k, key) == 0)
            return true;
        return right.containsKey(k);
    }

    public int size(){
        return size;
    }

    public boolean isEmpty(){
        return false;
    }

    public FMap<K,V> add(K k, V v){
        return ins(k, v).makeBlack();
    }

    public int hashCode(){
        int hash = key.hashCode() + value.hashCode();
        hash +=  left.hashCode();
        hash += right.hashCode();
        hash += 8;
        return hash;
    }

    public String toString(){
        return "{...(" + size() + " entries)...}";
    }

    public boolean equals(Object o){
        if(o instanceof FMap){
            FMap<K,V> tre = (FMap) o;
            ArrayList<K> allkeys = uniquekeys();
            allkeys.addAll(tre.uniquekeys());
            boolean same = true;
            for(int i = 0; i < allkeys.size(); i = i + 1)
            {
                K curr = allkeys.get(i);
                if(! (this.containsKey(curr) && tre.containsKey(curr)))
                    return false;
                if(!this.get(curr).equals(tre.get(curr)))
                    return false;
            }
            return true;
        }
        return false;
    }

    public FMap<K,V> accept(Visitor<K,V> v){
        return new Node(comp, col, key, v.visit(key, value),
            (FTree) left.accept(v), (FTree) right.accept(v));
    }
    public Iterator<K> iterator(){
        return new MapIt<K>(this.uniquekeys());
    }

}



class Empty<K,V> extends FMap<K,V>{

    //No instance variables exist

    //Constructor for the Empty implementation of FMap
    public Empty(){}

    public FMap<K,V> add(K k1, V v1){
        return new Add<K,V>(this, k1, v1);
    }

    public V get(K i){
        return null;
    }

    public boolean isEmpty(){
        return true;
    }

    public boolean containsKey(K key){
        return false;
    }

    public int size(){
        return 0;
    }

    public String toString(){
        return "{...(" + this.size() + " entries)...}";
    }

    public boolean equals(Object o){
        return (o instanceof FMap && ((FMap)o).isEmpty());
    }

    public int hashCode(){
        return 0;
    }

    public FMap<K,V> changed(K key, V val){
        return this;
    }

    public ArrayList<K> uniquekeys(){
        return new ArrayList<K>();
    }

    public FMap<K,V> accept(Visitor<K,V> v){
        return this;
    }

    public Iterator<K> iterator(){
        return new MapIt<K>(this.uniquekeys());
    }
}

class Add<K,V> extends FMap<K,V>{

    //Private instance variables. Size stored here to save on runtime
    int size;
    K key;
    V value;
    FMap<K,V> rest;

    //Constructor for an Add implementation.
    //Precomputes size and stores in an instance variable to save time.
    public Add(FMap<K,V> map, K k1, V v1){
        key = k1;
        value = v1;
        rest = map;
        size = 1 + rest.size();
    }

    public FMap<K,V> add(K k1, V v1){
        if(this.containsKey(k1))
            return this.changed(k1, v1);
        return new Add<K,V>(this, k1, v1);
    }

    public V get(K i){
        if (key.equals(i))
            return value;
        return rest.get(i);
    }

    public boolean containsKey(K theKey){
        if (key.equals(theKey))
            return true;
        return rest.containsKey(theKey);
    }

    public int size(){
        return size;
    }

    public boolean isEmpty(){
        return false;
    }

    public String toString(){
        return "{...(" + this.size() + " entries)...}";
    }

    FMap<K,V> changed(K setkey, V setval){
        if(key.equals(setkey))
            return rest.add(setkey, setval);
        return rest.changed(setkey, setval).add(key, value);
    }

    public int hashCode(){
        int hash = key.hashCode();
        hash += value.hashCode();
        hash += 8;
        return hash + rest.hashCode();
    }

    public boolean equals(Object o){
        if (o instanceof FMap)
        {
            @SuppressWarnings(value="unchecked")
            ArrayList total = this.uniquekeys();
            FMap<K,V> other = (FMap) o;
            total.addAll(other.uniquekeys());
            for(int i = 0; i < total.size(); i = i + 1)
            {
                K obj = (K) total.get(i);
                if(!this.containsKey(obj) || !other.containsKey(obj))
                    return false;
                if(!(this.get(obj)).equals(other.get(obj)))
                    return false;
            }
            return true;
        }
        return false;
    }

    public ArrayList<K> uniquekeys(){
        ArrayList<K> prevs = rest.uniquekeys();
        prevs.add(key);
        return prevs;
    }

    public FMap<K,V> accept(Visitor<K,V> v){
        return new Add(rest.accept(v), key, v.visit(key, value));
    }

    public Iterator<K> iterator(){
        return new MapIt<K>(this.uniquekeys());
    }
}



//Color for determining the color of a node in a red-black tree.
abstract class Color{

    //Abstract methods that determine if class is red or black
    abstract boolean isRed();
    abstract boolean isBlack();

    //Basic creators that make a red and a black
    static Color red(){
        return new Red();
    }

    static Color black(){
        return new Black();
    }

    static boolean isRed(Color c){
        return c.isRed();
    }

    static boolean isBlack(Color c){
        return c.isBlack();
    }
}

//Implementations of the Color class for Red and Black.
//When isRed is called, red return true, black returns false.
//When isBlack is called, red return fale, black return true/
class Red extends Color{
    boolean isRed(){
        return true;
    }

    boolean isBlack(){
        return false;
    }
}

class Black extends Color{
    boolean isRed(){ return false;}

    boolean isBlack(){return true;}
}

class MapIt<K> implements Iterator<K>{
    ArrayList<K> keys;
    int index;

    MapIt (ArrayList<K> input){
        keys = input;
        index = 0;
    }

    public boolean hasNext(){
        return index < keys.size();
    }

    public K next(){
        if(index >= keys.size())
            throw new NoSuchElementException("Too far!");
        K hold = keys.get(index);
        index = index + 1;
        return hold;
    }

    public void remove(){
        throw new UnsupportedOperationException();
    }
}