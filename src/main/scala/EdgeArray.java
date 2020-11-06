import scala.Array;
import scala.Byte;
import scala.Long;

import java.io.Serializable;

public class EdgeArray implements Serializable
{
    //private ArrayList<Long> edges;
    //private ArrayList<Character> labels;
    //private long[] edgess;
    //private ArrayList<Byte> labels;
    private int size;
    private int[] edges;
    //private char[] labelss;
    private byte[] labels;


    //public EdgeArray(ArrayList<Long> edges, ArrayList<Character> labels)
    //public EdgeArray(ArrayList<Integer> edges, ArrayList<Character> labels)
    //public EdgeArray(ArrayList<Integer> edges, ArrayList<Byte> labels)
    public EdgeArray(int size, int[] edges, byte[] labels)
    {
        //this.edgess = edges.stream().mapToLong(Long::valueOf).toArray();
        //this.edgess = edges.stream().mapToInt(Integer::valueOf).toArray();
        //this.edgess = Ints.toArray(edges);
        this.size = size;
        this.edges = edges;
        //System.out.print("数组： ");
        //System.out.println(Ints.toArray(edges).length);
        //System.out.println(Bytes.toArray(labels).length);
        //System.out.print("list： ");
        //System.out.println(Ints.asList(Ints.toArray(edges)).size());
        //this.labelss = Bytes.toArray(labels);
        this.labels = labels;
        //this.labelss = labels.stream().map(e->e.toString()).collect(Collectors.joining()).toCharArray();
        //this.labelss = labels.stream().mapToInt(Character::valueOf).toArray();
        //System.out.print("数组： ");
        //System.out.println(this.labelss.length);
        //this.labelss = labels.stream().mapto
        //this.edges = edges;
        //this.labels = labels;
        //System.out.print("list： ");
        //System.out.println(labels.size());
    }

    public EdgeArray()
    {
        //edges = new ArrayList<>();
        //labels = new ArrayList<>();
        this.size = 0;
        this.edges = new int[0];
        this.labels = new byte[0];
        //edgess = new long[0];
        //this.edgess = edges.stream().mapToLong(Long::valueOf).toArray();
    }
    public int getSize()
    {
        return this.size;
    }
    public boolean isEmpty()
    {
        return this.size == 0;
    }
    //public ArrayList<Long> getEdges()
    public int[] getEdges()
    {
        //return (ArrayList<Long>) Arrays.stream(this.edgess).boxed().collect(Collectors.toList());
        //return (ArrayList<Integer>) Arrays.stream(this.edgess).boxed().collect(Collectors.toList());
        //return new ArrayList(Ints.asList(this.edges));
        return this.edges;
    }

    //public ArrayList<Character> getLabels()
    public byte[] getLabels()
    //public ArrayList<Byte> getLabels()
    {
        //return (ArrayList<Character>)Arrays.stream(this.labelss).boxed().collect(Collectors.toList());
        //Arrays.stream(this.labelss)
        //String myString = String.valueOf(this.labelss);
        //return (ArrayList<Character>) myString.chars().mapToObj(c->(char)c).collect(Collectors.toList());
        //return (ArrayList<Character>) myStreamOfCharacters.collect(Collectors.toList());
        //System.out.print("数组： ");
        //System.out.println(this.labelss.length);
        //System.out.print("list： ");
        //System.out.println(this.labels.size());
        //return Arrays.asList(this.labelss);
        //return this.labels;
        return this.labels;
        //return new ArrayList(Bytes.asList(this.labelss));
        //return (ArrayList<Byte>) Bytes.asList(this.labelss);
    }

    @Override
    public boolean equals(Object objp)
    {
/*		if(objp == null)
			return false;*/
        EdgeArray obj = (EdgeArray)objp;
        if(this == obj)
            return true;
        //int size = this.edges.size();
        //int size = this.size;
        //if(size != obj.edges.size())
        if(this.size != obj.size)
            return false;
        for(int i = 0; i < this.size; i++)
        {
            //if(!this.getEdges().get(i).equals(obj.getEdges().get(i)) || !this.getLabels().get(i).equals(obj.getLabels().get(i)))
            if((this.getEdges()[i] != obj.getEdges()[i]) || (this.getLabels()[i] != (obj.getLabels()[i])))
                return false;
        }
        return true;
    }
    //deep copy
    public void dcopy(EdgeArray edgearray)
    {
        //ArrayList<Long> tmpedges = new ArrayList<>();
        //ArrayList<Integer> tmpedges = new ArrayList<>();
        //ArrayList<Character> tmplabels = new ArrayList<>();
        //ArrayList<Byte> tmplabels = new ArrayList<>();
        //int size = ;
        int[] tmp_edges = new int[edgearray.size];
        byte[] tmp_labels = new byte[edgearray.size];
        //ArrayList<Long> edge = edgearray.getEdges();
        //ArrayList<Integer> edge = edgearray.getEdges();
        int[] edge = edgearray.getEdges();
        //ArrayList<Character> label = edgearray.getLabels();
        //ArrayList<Byte> label = edgearray.getLabels();
        byte[] label = edgearray.getLabels();
        for(int i = 0; i < edgearray.size; i++)
        {
            //Long e = edge.get(i);
            //Integer e = edge.get(i);
            int e = edge[i];
            //Character l = label.get(i);
            //Byte l = label.get(i);
            byte l = label[i];
            //tmpedges.add(e);
            //tmplabels.add(l);
            tmp_edges[i] = e;
            tmp_labels[i] = l;
        }
        //this.edges = tmpedges;
        //this.edgess = tmpedges.stream().mapToLong(Long::valueOf).toArray();
        //this.edgess = tmpedges.stream().mapToInt(Integer::valueOf).toArray();
        //this.edges = Ints.toArray(tmpedges);
        this.size = edgearray.size;
        this.edges = tmp_edges;
        this.labels = tmp_labels;
        //this.labelss = Bytes.toArray(tmplabels);
        //this.labelss = tmplabels.toString().toCharArray();
        //this.labelss = tmplabels.stream().map(e->e.toString()).collect(Collectors.joining()).toCharArray();
        //this.labels = tmplabels;
    }
}
