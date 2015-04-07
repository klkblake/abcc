import org.graphstream.graph.Edge;
import org.graphstream.graph.Graph;
import org.graphstream.graph.Node;
import org.graphstream.graph.implementations.SingleGraph;
import org.graphstream.ui.swingViewer.Viewer;

import java.lang.reflect.InvocationTargetException;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.SwingUtilities;

/**
 * Created by kyle on 3/04/15.
 */
public class Main {
    static Pattern nodePattern = Pattern.compile("([^ ]*) *\\[label=\"(.*)\"\\]");
    static Pattern edgePattern = Pattern.compile("([^ ]*) *-> *([^ ]*) *\\[label=\"(.*)\"\\]");
    static String styleSheet =
            "node {" +
            "    shape: box;" +
            "    size-mode: fit;" +
            "    padding: 4;" +
            "    fill-color: white;" +
            "    stroke-mode: plain;" +
            '}' +
            "node.root {" +
            "    fill-color: red;" +
            '}' +
            "edge {" +
            "    shape: cubic-curve;" +
            "    text-alignment: along;" +
            "    text-background-mode: plain;" +
            '}';

    public static void main(String[] args) throws IOException, InterruptedException, InvocationTargetException {
        ServerSocket ssock = new ServerSocket();
        ssock.setReuseAddress(true);
        ssock.bind(new InetSocketAddress(Integer.parseInt(args[0])));
        Socket sock = ssock.accept();
        BufferedReader in = new BufferedReader(new InputStreamReader(sock.getInputStream()));
        System.setProperty("org.graphstream.ui.renderer", "org.graphstream.ui.j2dviewer.J2DGraphRenderer");
        //System.setProperty("org.graphstream.ui.layout", "org.graphstream.ui.layout.Eades84Layout");
        System.setProperty("org.graphstream.ui.layout", "Layout");
        Graph g = new SingleGraph("graph");
        g.addAttribute("ui.stylesheet", styleSheet);
        g.addAttribute("ui.quality");
        g.addAttribute("ui.antialias");
        g.addAttribute("layout.quality", 1);
        g.addAttribute("layout.stabilization-limit", 0.999f);
        g.setAutoCreate(true);
        g.setStrict(false);
        final Viewer viewer = g.display(false);
        Set<String> nodesToRemove = new HashSet<>();
        Set<String> edgesToRemove = new HashSet<>();
        boolean seenRoot = false;
        int stage = 0;
        String line;
        while ((line = in.readLine()) != null) {
            line = line.trim();
            if (line.startsWith("digraph ") || line.startsWith("subgraph ")) {
	            seenRoot = false;
                if (stage == 1) {
                    stage = 2;
                    g.addAttribute("layout.force", 0.01f);
                }
                if (stage == 0) {
                    stage = 1;
                }
                SwingUtilities.invokeAndWait(new Runnable() {
	                public void run() {
		                viewer.disableAutoLayout();
	                }
		});
            } else {
                if (line.equals("}")) {
                    for (String id : nodesToRemove) {
                        g.removeNode(id);
                    }
                    for (String id : edgesToRemove) {
                        g.removeEdge(id);
                    }
                    if (g.getNodeCount() == 0 && g.getEdgeCount() == 0) {
                        stage = 1;
                        g.addAttribute("layout.force", 1);
                    }
                    initToRemove(nodesToRemove, edgesToRemove, g);
                    SwingUtilities.invokeAndWait(new Runnable() {
	                    public void run() {
		                    viewer.enableAutoLayout();
	                    }
                    });
                } else {
                    Matcher edgeMatcher = edgePattern.matcher(line);
                    Matcher nodeMatcher = nodePattern.matcher(line);
                    if (nodeMatcher.matches()) {
                        Node node = g.addNode(nodeMatcher.group(1));
                        node.addAttribute("ui.label", deescape(nodeMatcher.group(2)));
                        if (!seenRoot) {
                            seenRoot = true;
                            node.addAttribute("ui.class", "root");
                        }
                        nodesToRemove.remove(nodeMatcher.group(1));
                    } else if (edgeMatcher.matches()) {
                        String from = edgeMatcher.group(1);
                        String to = edgeMatcher.group(2);
                        // XXX self-loops are busted
                        assert from != null;
                        if (!from.equals(to) || !(edgeMatcher.group(3).equals("rep") || edgeMatcher.group(3).equals("next"))) {
                            String id = from + " -> " + to;
                            g.addEdge(id, from, to, true).addAttribute("ui.label", deescape(edgeMatcher.group(3)));
                            edgesToRemove.remove(id);
                        }
                    } else {
                        System.err.println("Illegal line: " + line);
                    }
                }
            }
        }
    }

    private static void initToRemove(Set<String> nodesToRemove, Set<String> edgesToRemove, Graph g) {
        nodesToRemove.clear();
        for (Node node : g.getEachNode()) {
            nodesToRemove.add(node.getId());
        }
        edgesToRemove.clear();
        for (Edge edge : g.getEachEdge()) {
            edgesToRemove.add(edge.getId());
        }
    }

    private static String deescape(String str) {
        return str.replace("\\\"", "\"");
    }
}
