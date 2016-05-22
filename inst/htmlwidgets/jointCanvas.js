HTMLWidgets.widget({

  name: 'jointCanvas',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {

      renderValue: function(x) {

        // define a div to hold the widget
        var id = el.id;
        var paperId = id + '-paper';
        var p = document.createElement('div');
        p.id = paperId;
        el.appendChild(p);


        // define the graph
        var graph = new joint.dia.Graph;

        // define the paper and assign to div element
        var paper = new joint.dia.Paper({
        el: p,
        width: 600,
        height: 200,
        model: graph,
        gridSize: x.gridSize,
        markAvailable: x.markAvailable,
        restrictTranslate: x.restrictTranslate,
        multiLinks: x.multiLinks
    });

    // create a couple of rectangles and join them
/*    var rect = new joint.shapes.basic.Rect({
        position: { x: 100, y: 30 },
        size: { width: 100, height: 30 },
        attrs: { rect: { fill: 'blue' }, text: { text: 'my box', fill: 'white' } }
    });

    var rect2 = rect.clone();
    rect2.translate(300);

    var link = new joint.dia.Link({
        source: { id: rect.id },
        target: { id: rect2.id }
    });

    graph.addCells([rect, rect2, link]);
*/


        var m1 = new joint.shapes.devs.Model({
        position: { x: 50, y: 50 },
        size: { width: 90, height: 90 },
        inPorts: ['in1','in2'],
        outPorts: ['out'],
        attrs: {
            '.label': { text: 'Model', 'ref-x': .4, 'ref-y': .2 },
            rect: { fill: '#2ECC71' },
            '.inPorts circle': { fill: '#16A085' },
            '.outPorts circle': { fill: '#E74C3C' }
        }
    });
    var m2 = m1.clone();
    m2.translate(300, 0);
    graph.addCells([m1, m2]);


      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
