
// define the graphs
var graph = new joint.dia.Graph;
var stencilGraph = new joint.dia.Graph;

// Add a single node
Shiny.addCustomMessageHandler("createNode",
  function(data) {
    var node = new joint.shapes.devs.Model({
      position: { x: data.x, y: data.y },
      size: { width: 100, height: 30 },
      inPorts: ['in1'],
      outPorts: ['out'],
      ports: {
          groups: {
              'in': {
                  position: "top",
                  attrs: {
                      '.port-body': {
                          r: "6",
                          fill: 'blue',
                          magnet: 'passive'
                      },
                    '.port-label': {
                      fill: "transparent"
                    }
                  }
              },
              'out': {
                  position: "bottom",
                  portLabelMarkup: '<text fill="yellow"/>',
                  attrs: {
                      '.port-body': {
                          r: "6",
                          fill: 'red'
                      },
                      '.port-label': {
                        fill: "transparent"
                    }
                  }
              }
          }
      },
      attrs: {
          rect: { fill: 'LightGrey', rx: 15, ry: 15 },
          text: { text: data.name }
      },
    });
    node.prop('nodeType', data.name);
    stencilGraph.addCell(node);
  }
);



HTMLWidgets.widget({

  name: 'jointPipeline',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {

      renderValue: function(x) {

        // define a div to hold the widget
        // div contains left and right panes to hold nodes and construction area
        var id = el.id;
        var div_all = document.createElement('div');
        var div_stencil = document.createElement('div');
        var div_paper = document.createElement('div');
        div_all.id = id + '-all';
        div_stencil.id = id + '-stencil';
        div_stencil.classList.add('div_stencil');
        div_paper.id = id + '-paper';
        div_paper.classList.add('div_paper');
        div_all.appendChild(div_stencil);
        div_all.appendChild(div_paper);
        el.appendChild(div_all);

        var outputSelectedNode = id + '_selectedNode:nodeOut';

        // define the paper and assign to div element
        paper = new joint.dia.Paper({
          el: $('#' + div_paper.id),
          model: graph,
          defaultLink: new joint.dia.Link({
            attrs: { '.marker-target': { d: 'M 10 0 L 0 5 L 10 10 z' } }
          }),
          validateConnection: function(cellViewS, magnetS, cellViewT, magnetT, end, linkView) {
            // Prevent linking from input ports.
            if (magnetS && magnetS.getAttribute('port-group') === 'in') return false;
            // Prevent linking from output ports to input ports within one element.
            if (cellViewS === cellViewT) return false;
            // Prevent linking to input ports.
            return magnetT && magnetT.getAttribute('port-group') === 'in';
          },
          // Enable marking available cells & magnets
          markAvailable: true,
          // Enable link snapping within 75px lookup radius
          snapLinks: { radius: 75 }
        });

        // drag and drop code taken from SO post
        // http://stackoverflow.com/questions/31283895/joint-js-drag-and-drop-element-between-two-papers
        // Canvas for node storage
        var stencilPaper = new joint.dia.Paper({
          el: $('#' + div_stencil.id),
          model: stencilGraph,
          interactive: false
        });

        // Events for drag and drop from first pane to second pane
        stencilPaper.on('cell:pointerdown', function(cellView, e, x, y) {
          $('body').append('<div id="flyPaper" style="position:fixed;z-index:100;opacity:.7;pointer-event:none;"></div>');
          var flyGraph = new joint.dia.Graph,
            flyPaper = new joint.dia.Paper({
              el: $('#flyPaper'),
              model: flyGraph,
              height: 30,
              width: 100,
              interactive: false
            }),
            flyShape = cellView.model.clone(),
            pos = cellView.model.position(),
            offset = {
              x: x - pos.x,
              y: y - pos.y
            };

          flyShape.position(0, 0);
          flyGraph.addCell(flyShape);
          $("#flyPaper").offset({
            left: e.pageX - offset.x,
            top: e.pageY - offset.y
          });
          $('body').on('mousemove.fly', function(e) {
            $("#flyPaper").offset({
              left: e.pageX - offset.x,
              top: e.pageY - offset.y
            });
          });
          $('body').on('mouseup.fly', function(e) {
            var x = e.pageX,
              y = e.pageY,
              target = paper.$el.offset();

            // Dropped over paper ?
            if (x > target.left && x < target.left + paper.$el.width() && y > target.top && y < target.top + paper.$el.height()) {
              var s = flyShape.clone();
              s.position(x - target.left - offset.x, y - target.top - offset.y);
              graph.addCell(s);
            }
            $('body').off('mousemove.fly').off('mouseup.fly');
            flyShape.remove();
            $('#flyPaper').remove();
          });
        });

        // paper events
        paper.on('cell:pointerclick', function(cellView, evt, x, y) {
          out = {};
          out.id = cellView.model.id;
          out.type = cellView.model.prop('nodeType');
          Shiny.onInputChange(outputSelectedNode, out);
        });


        var outputId = id + '_graph:linksTable';

        // update pipeline output on any event
        graph.on('add change remove', function(eventName, cell) {
          var pipelineLinks = graph.getLinks();
          if (pipelineLinks.length > 0) {
            var output = [];
            for (var i in pipelineLinks) {
              var link = pipelineLinks[i];
              output.push({
                "id" : link.id,
                "source_id" : link.get('source').id,
                "source_type" : link.getSourceElement().prop('nodeType'),
                "source_port" : link.get('source').port,
                // need to check for target element in case as event is fired during link creation
                "target_id" : link.getTargetElement() === null ? '' : link.get('target').id,
                "target_type" : link.getTargetElement() === null ? '' : link.getTargetElement().prop('nodeType'),
                "target_port" : link.getTargetElement() === null ? '' : link.get('target').port
              });
            }
            Shiny.onInputChange(outputId, output);
          } else {
            Shiny.onInputChange(outputId, null);
          }
        });

      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
