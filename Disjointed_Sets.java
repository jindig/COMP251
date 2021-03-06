java.util

public class DisjointSets {

    private int[] par;
    
    /* contructor: creates a partition of n elements. */
    /* Each element is a separate disjoint set */
    DisjointSets(int n) {
        if (n>0) {
            par = new int[n];
            for (int i=0; i<this.par.length; i++) {
                par[i] = i;
            }
        }
    }
    
    public String toString(){
        int pari,countsets=0;
        String output = "";
        String[] setstrings = new String[this.par.length];
        /* build string for each set */
        for (int i=0; i<this.par.length; i++) {
            pari = find(i);
            if (setstrings[pari]==null) {
                setstrings[pari] = String.valueOf(i);
                countsets+=1;
            } else {
                setstrings[pari] += "," + i;
            }
        }
        /* print strings */
        output = countsets + " set(s):\n";
        for (int i=0; i<this.par.length; i++) {
            if (setstrings[i] != null) {
                output += i + " : " + setstrings[i] + "\n";
            }
        }
        return output;
    }
    
    /* find resentative of element i */
    public int find(int i) {

        if (this.par[i] == i) {
        	// Check if i is the representative of its set. If so, return it
        	return i;
        } else {
        	// Set the parent of i to be the representative of the set to which it belongs
        	// This recursively compresses the tree such that all members are
        	// children of the representative
        	this.par[i] = find(this.par[i]);
        	return this.par[i];
        }
        
    }

    /* merge sets containing elements i and j */
    public int union(int i, int j) {
    
        if (this.find(i) != this.find(j)) {
        	// If i and j do not belong to the same set, then set the representative of set i
        	// to be a child of the representative of set j
        	this.par[this.find(i)] = this.find(j);
        }
        
        return this.par[this.find(i)];
    }
    
    public static void main(String[] args) {
        
        DisjointSets myset = new DisjointSets(6);
        System.out.println(myset);
        System.out.println("-> Union 2 and 3");
        myset.union(2,3);
        System.out.println(myset);
        System.out.println("-> Union 2 and 3");
        myset.union(2,3);
        System.out.println(myset);
        System.out.println("-> Union 2 and 1");
        myset.union(2,1);
        System.out.println(myset);
        System.out.println("-> Union 4 and 5");
        myset.union(4,5);
        System.out.println(myset);
        System.out.println("-> Union 3 and 1");
        myset.union(3,1);
        System.out.println(myset);
        System.out.println("-> Union 2 and 4");
        myset.union(2,4);
        System.out.println(myset);
        
    }

}
