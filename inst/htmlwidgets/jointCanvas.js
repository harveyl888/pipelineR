
// define the graph
var graph = new joint.dia.Graph;

Shiny.addCustomMessageHandler("addEl",
  function(data) {
       var m1 = new joint.shapes.devs.Model({
        position: { x: data.x, y: data.y },
        size: { width: 90, height: 90 },
        inPorts: ['in1','in2'],
        outPorts: ['out'],
        attrs: {
            '.label': { text: data.name, 'ref-x': 0.4, 'ref-y': 0.2 },
            rect: { fill: '#2ECC71' },
            '.inPorts circle': { fill: '#16A085', magnet: 'passive', type: 'input' },
            '.outPorts circle': { fill: '#E74C3C', type: 'output' }
        },
        prop: {nodeType: 'node 1'}


    });
        graph.addCell(m1);

    }
);


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

        if (x.border === true) {
          p.setAttribute("style", "border:1px solid black;");
        }

        // define the paper and assign to div element
        var paper = new joint.dia.Paper({
          el: p,
          width: el.offsetWidth,
          height: el.offsetHeight,
          model: graph,
          gridSize: x.gridSize,
          markAvailable: x.markAvailable,
          restrictTranslate: x.restrictTranslate,
          multiLinks: x.multiLinks,
          validateConnection: function(cellViewS, magnetS, cellViewT, magnetT, end, linkView) {
            // Prevent linking from input ports.
            if (magnetS && magnetS.getAttribute('type') === 'input') return false;
            // Prevent linking from output ports to input ports within one element.
            if (cellViewS === cellViewT) return false;
            // Prevent linking to input ports.
            return magnetT && magnetT.getAttribute('type') === 'input';
          }
      });


/*        var m1 = new joint.shapes.devs.Model({
        position: { x: 50, y: 50 },
        size: { width: 90, height: 90 },
        inPorts: ['in1','in2'],
        outPorts: ['out'],
        attrs: {
            '.label': { text: 'Model 1', 'ref-x': 0.4, 'ref-y': 0.2 },
            rect: { fill: '#2ECC71' },
            '.inPorts circle': { fill: '#16A085', magnet: 'passive', type: 'input' },
            '.outPorts circle': { fill: '#E74C3C', type: 'output' }
        },
        prop: {nodeType: 'node 1'}
    });

    var m2 = m1.clone();
    m2.translate(300, 0).attr('.label/text', 'Model 2');
    m2.prop('nodeType', 'node 2');
    graph.addCells([m1, m2]);

*/

    var outputId = id + '_pipeline';

    // update pipeline output on any event
    graph.on('all', function(eventName, cell) {
      var pipelineLinks = graph.getLinks();
      var output = {
          pipeline: []
      };
      for (var i in pipelineLinks) {
        var link = pipelineLinks[i];
        output.pipeline.push({
          "source_id" : link.get('source').id,
          "source_type" : 3,
          "source_port" : link.get('source').port,
          "target_id" : link.get('target').id,
          "target_port" : link.get('target').port
        });
      }

      Shiny.onInputChange(outputId, output);
    });

      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
