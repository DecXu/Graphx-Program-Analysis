import java.util.HashMap;

public class Tool
{
    /*static void print(HashMap<Long, EdgeArray> out)
    {
        for(Long k : out.keySet())
        {
            System.out.print(k);
            System.out.print(' ');
            System.out.print(out.get(k).getSize());
            System.out.print(' ');
            for(int i = 0; i < out.get(k).getSize(); i++)
            {
                System.out.print(out.get(k).getEdges()[i]);
                System.out.print(' ');
                System.out.print(out.get(k).getLabels()[i]);
                System.out.print(' ');
            }
        }
        System.out.println(' ');
    }*/
    /*static void changetoString(StringBuilder str, HashMap<Long, EdgeArray> in)
    {
        // TODO Auto-generated method stub
        str.append(in.size()).append(' ');
        for(Long key : in.keySet())
        {
            EdgeArray tmp = in.get(key);
            //str.append(tmp.getEdges().size()).append(' ');
            int size = tmp.getSize();
            //System.out.println(size);
            str.append(size).append(' ');
            //for(int i = 0; i < tmp.getEdges().size(); i++)
            for(int i = 0; i < size; i++)
            {
                //str.append(tmp.getEdges().get(i)).append(' ');
                //str.append((int)(tmp.getLabels().get(i))).append(' ');
                str.append(tmp.getEdges()[i]).append(' ');
                str.append((tmp.getLabels()[i])).append(' ');
            }
            str.append((key)).append(' ');
        }
        //System.out.println(str.toString());
    }*/

    static void dcopy(HashMap<Long, EdgeArray> prepegraph, HashMap<Long, EdgeArray> map)
    {
        // TODO Auto-generated method stub
        for(Long key : map.keySet())
        {
            EdgeArray tmp = new EdgeArray();
            EdgeArray k = map.get(key);
            tmp.dcopy(k);
            prepegraph.put(key, tmp);
        }
    }

    static void update_graphstore(HashMap<Long, HashMap<Long, EdgeArray>> result, HashMap<Long, HashMap<Long, EdgeArray>> msg)
    {
        for(Long key : msg.keySet())
        {
            result.put(key,msg.get(key));
        }
    }

    static void getin(HashMap<Long, EdgeArray> in, HashMap<Long, HashMap<Long, EdgeArray>> graphstore)
    {
        for(Long key : graphstore.keySet())
        {
            HashMap<Long, EdgeArray> prepegraph = new HashMap<>();
            dcopy(prepegraph, graphstore.get(key));
            merge(in, prepegraph);
        }
    }

    static void merge(HashMap<Long, EdgeArray> in, HashMap<Long, EdgeArray> message)
    {
        //scala.Long k = new scala.Long(2l);
        for(Long key : message.keySet())
        {
            if(!in.containsKey(key))
            {
                in.put(key, message.get(key));
            }
            else
            {
                //ArrayList<Long> edges_tmp = new ArrayList<>();
                //ArrayList<Integer> edges_tmp = new ArrayList<>();
                //ArrayList<Character> labels_tmp = new ArrayList<>();
                //ArrayList<Byte> labels_tmp = new ArrayList<>();
                //int n1 = message.get(key).getEdges().size();
                //int n2 = in.get(key).getEdges().size();
                int n1 = message.get(key).getSize();
                int n2 = in.get(key).getSize();
                int[] edges_tmp = new int[n1 + n2];
                byte[] labels_tmp = new byte[n1 + n2];

                EdgeArray in_array = in.get(key);
                EdgeArray message_array = message.get(key);
                int size = unionTwoArray(edges_tmp, labels_tmp, in_array, message_array);
                //System.out.println(size);
                EdgeArray result = new EdgeArray(size, edges_tmp, labels_tmp);
                in.put(key, result);
            }
        }
    }

    //static void unionTwoArray(ArrayList<Integer> dstA, ArrayList<Byte> dstB, EdgeArray in_array,EdgeArray message_array)
    static int unionTwoArray(int[] dstA, byte[] dstB, EdgeArray in_array, EdgeArray message_array)
    {
        // TODO Auto-generated method stub
        int len = 0;
        //1是message的内容，2是in的内容。A是edge，B是label
        int len1 = message_array.getSize();
        int len2 = in_array.getSize();
        //ArrayList<Long> A1 = message_array.getEdges();
        //ArrayList<Long> A2 = in_array.getEdges();
        //ArrayList<Integer> A1 = message_array.getEdges();
        //ArrayList<Integer> A2 = in_array.getEdges();
        //ArrayList<Character> B1 = message_array.getLabels();
        //ArrayList<Character> B2 = in_array.getLabels();
        //ArrayList<Byte> B1 = (ArrayList<Byte>) message_array.getLabels();
        //ArrayList<Byte> B2 = (ArrayList<Byte>) in_array.getLabels();

        int[] A1 = message_array.getEdges();
        int[] A2 = in_array.getEdges();
        byte[] B1 = message_array.getLabels();
        byte[] B2 = in_array.getLabels();

        if(len1 > 0)
        {
            if(len2 > 0)
            {
                int p1 = 0; int p2 = 0;
                while(p1 < len1 && p2 < len2)
                {
                    //long value = myCompare(A1.get(p1),B1.get(p1),A2.get(p2),B2.get(p2));
                    //int value = myCompare(A1.get(p1),B1.get(p1),A2.get(p2),B2.get(p2));
                    int value = myCompare(A1[p1],B1[p1],A2[p2],B2[p2]);
                    if(value > 0)
                    {
                        //dstA.add(len, A2.get(p2));
                        //dstB.add(len, B2.get(p2));
                        dstA[len] = A2[p2];
                        dstB[len] = B2[p2];
                        ++p2; ++len;
                    }
                    else if(value < 0)
                    {
                        //dstA.add(len, A1.get(p1));
                        //dstB.add(len, B1.get(p1));
                        dstA[len] = A1[p1];
                        dstB[len] = B1[p1];
                        ++p1; ++len;
                    }
                    else
                    {
                        //dstA.add(len, A1.get(p1));
                        //dstB.add(len, B1.get(p1));
                        dstA[len] = A1[p1];
                        dstB[len] = B1[p1];
                        ++p1; ++p2; ++len;
                    }
                }
                while(p1 < len1)
                {
                    //dstA.add(len, A1.get(p1));
                    //dstB.add(len, B1.get(p1));
                    dstA[len] = A1[p1];
                    dstB[len] = B1[p1];
                    ++p1; ++len;
                }
                while(p2 < len2)
                {
                    //dstA.add(len, A2.get(p2));
                    //dstB.add(len, B2.get(p2));
                    dstA[len] = A2[p2];
                    dstB[len] = B2[p2];
                    ++p2; ++len;
                }
            }
            else
            {
                //dstA.addAll(A1);
                len = len1;
                System.arraycopy(A1, 0, dstA, 0, len);
                System.arraycopy(B1, 0, dstB, 0, len);
                //dstB = Arrays.copyOf(B1, len);
                //dstB.addAll(B1);
            }
        }
        else
        {
            if(len2 > 0)
            {
                //dstA.addAll(A2);
                //dstB.addAll(B2);
                len = len2;
                System.arraycopy(A2, 0, dstA, 0, len);
                System.arraycopy(B2, 0, dstB, 0, len);
            }
        }
        return len;
    }

    static int myCompare(int v1, byte l1, int v2, byte l2)
    {
        // TODO Auto-generated method stub
        return (v1 == v2) ? (l1 - l2) : (v1 - v2);
    }

    /*static void changetoMap(HashMap<Long, EdgeArray> out, String test2)
    {
        String[] test;
        //String delimeter = " ";
        test = test2.split(" ");
        int index = 0;
        int num1 = Integer.parseInt(test[index++]);
        for(int i = 0; i < num1; i++)
        {
            int num2 = Integer.parseInt(test[index++]);
            //ArrayList<Long> edges2 = new ArrayList<>();
            //ArrayList<Integer> edges2 = new ArrayList<>();
            //ArrayList<Character> labels2 = new ArrayList<>();
            //ArrayList<Byte> labels2 = new ArrayList<>();
            int[] edges2 = new int[num2];
            byte[] labels2 = new byte[num2];

            for(int k = 0; k < num2; k++)
            {
                //edges2.add(Long.parseLong(test[index++]));
                //edges2.add(Integer.parseInt(test[index++]));
                //labels2.add((char)(Integer.parseInt(test[index++]) + 129));
                //labels2.add((byte)(Integer.parseInt(test[index++])));
                edges2[k] = Integer.parseInt(test[index++]);
                labels2[k] = Byte.parseByte(test[index++]);
            }
            //System.out.println(num2);
            EdgeArray msg2 = new EdgeArray(num2, edges2, labels2);
            //System.out.println(msg2.getSize());
            out.put((long) Integer.parseInt(test[index++]), msg2);
        }
    }*/

    static boolean isEquals(HashMap<Long, EdgeArray> out, HashMap<Long, EdgeArray> pegraph)
    {
        // TODO Auto-generated method stub
        //if(label == 0)
        if(pegraph.containsKey(-1l))
        {
            return false;
        }
        if(out == pegraph)
            return true;
        if(out.size() != pegraph.size())
            return false;
        for(Long tmp : out.keySet())
        {
            if(!pegraph.containsKey(tmp))
                return false;
            else
            {
                EdgeArray a1 = out.get(tmp);
                EdgeArray a2 = pegraph.get(tmp);
                if(!a1.equals(a2))
                    return false;
            }
        }
        return true;
    }

    static int sum_edge(HashMap<Long, EdgeArray> k)
    {
        int sum = 0;
        for(Long tmp : k.keySet())
        {
            sum += k.get(tmp).getSize();
        }
        return sum;
    }
}
