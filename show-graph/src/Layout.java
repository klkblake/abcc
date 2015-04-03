import org.graphstream.ui.layout.springbox.EdgeSpring;
import org.graphstream.ui.layout.springbox.NodeParticle;
import org.graphstream.ui.layout.springbox.implementations.SpringBox;

/**
 * Created by kyle on 3/04/15.
 */
public class Layout extends SpringBox {
    @Override
    public String getLayoutAlgorithmName() {
        return "ContinuousSpringBoxLayout";
    }

    @Override
    protected void addEdge(String sourceId, String id, String from, String to, boolean directed) {
        NodeParticle n0 = (NodeParticle) nodes.getParticle(from);
        NodeParticle n1 = (NodeParticle) nodes.getParticle(to);

        if (n0 != null && n1 != null) {
            EdgeSpring e = new EdgeSpring(id, n0, n1);
            EdgeSpring o = edges.put(id, e);

            if (o != null) {
                // throw new SingletonException(
                // "edge '"+id+"' already exists");
                System.err.printf("layout %s: edge '%s' already exists%n",
                        getLayoutAlgorithmName(), id);
            } else {
                n0.registerEdge(e);
                n1.registerEdge(e);
            }

            //chooseNodePosition(n0, n1);
        } else {
            if (n0 == null)
                System.err
                        .printf("layout %s: node '%s' does not exist, cannot create edge %s%n",
                                getLayoutAlgorithmName(), from, id);
            if (n1 == null)
                System.err
                        .printf("layout %s: node '%s' does not exist, cannot create edge %s%n",
                                getLayoutAlgorithmName(), to, id);
        }
    }
}
