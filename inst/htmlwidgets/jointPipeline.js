
// define the graphs
var graph = new joint.dia.Graph;
var stencilGraph = new joint.dia.Graph;

// node counter
var counter = 1;

// Define a new shape to take an html layer
joint.shapes.devs.PipelineNode = joint.shapes.devs.Model.extend({
  defaults: joint.util.deepSupplement({
    type: 'devs.PipelineNode'
  }, joint.shapes.devs.Model.prototype.defaults)
});

// Custom view
joint.shapes.devs.PipelineNodeView = joint.dia.ElementView.extend({
  template: [
    '<div class="html-element">',
    '<button class="delete">x</button>',
    '</div>'
  ].join(''),
  initialize: function() {
    _.bindAll(this, 'updateBox');
    joint.dia.ElementView.prototype.initialize.apply(this, arguments);

    this.$box = $(_.template(this.template)());
    // Prevent paper from handling pointerdown.
    this.$box.find('input,select').on('mousedown click', function(evt) {
      evt.stopPropagation();
    });

    // This is an example of reacting on the input change and storing the input data in the cell model.
    this.$box.find('input').on('change', _.bind(function(evt) {
      this.model.set('input', $(evt.target).val());
    }, this));
    this.$box.find('select').on('change', _.bind(function(evt) {
      this.model.set('select', $(evt.target).val());
    }, this));
    this.$box.find('select').val(this.model.get('select'));
    this.$box.find('.delete').on('click', _.bind(this.model.remove, this.model));
    // Update the box position whenever the underlying model changes.
    this.model.on('change', this.updateBox, this);
    // Remove the box when the model gets removed from the graph.
    this.model.on('remove', this.removeBox, this);

    this.updateBox();
  },
  render: function() {
    joint.dia.ElementView.prototype.render.apply(this, arguments);
    this.paper.$el.prepend(this.$box);
    this.updateBox();
    return this;
  },
  updateBox: function() {
      // Set the position and dimension of the box so that it covers the JointJS element.
      var bbox = this.model.getBBox();

      // Define visibility of delete button
      this.$box.find('button').toggleClass('invisible', this.model.get('hideDeleteButton'));

      this.$box.css({
        width: bbox.width,
        height: bbox.height,
        left: bbox.x,
        top: bbox.y,
        transform: 'rotate(' + (this.model.get('angle') || 0) + 'deg)'
      });
  },
  removeBox: function(evt) {
    this.$box.remove();
  }
});


// Add a single node
Shiny.addCustomMessageHandler("createNode",
  function(data) {
    var node = new joint.shapes.devs.PipelineNode({
      position: { x: data.x, y: data.y },
      size: { width: 100, height: 30 },
      hideDeleteButton : true,
      inPorts: Array.apply(null, Array(data.ports_in)).map(function (_, i) {return ('in' + (i+1));}),
      outPorts: Array.apply(null, Array(data.ports_out)).map(function (_, i) {return ('out' + (i+1));}),
      hasInputPort : data.ports_in > 0,
      hasOutputPort : data.ports_out > 0,
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
    node.prop('nodeName', '');
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
        var outputLastDroppedNode = id + '_lastDroppedNode:nodeOut';

        // define the paper and assign to div element
        paper = new joint.dia.Paper({
          el: $('#' + div_paper.id),
          height: height,
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
          height: height,
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
              var s = flyShape.clone();  // clone the element
              s.set('hideDeleteButton', false);  // show delete button
              s.position(x - target.left - offset.x, y - target.top - offset.y);
              s.prop('nodeName', s.prop('nodeType') + "_" + counter);  // unique node name
              counter ++;
              graph.addCell(s);
              // update shiny variable holding node info
              out = {};
              out.id = s.id;
              out.type = s.prop('nodeType');
              out.name = s.prop('nodeName');
              Shiny.onInputChange(outputLastDroppedNode, out);
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
          out.name = cellView.model.prop('nodeName');
          Shiny.onInputChange(outputSelectedNode, out);
        });


        var outputNodes = id + '_nodes:linksTable';
        var outputLinks = id + '_links:linksTable';
        var outputDFS = id + '_dfs';

        // update pipeline output on any event
        graph.on('add change remove', function(eventName, cell) {
          var pipelineNodes = graph.getElements();
          if(pipelineNodes.length > 0) {
            var outNodes = [];
            for (var iNode in pipelineNodes) {
              var node = pipelineNodes[iNode];
              outNodes.push({
                "id" : node.id,
                "type" : node.prop("nodeType"),
                "name" : node.prop("nodeName")
              });
            }
            Shiny.onInputChange(outputNodes, outNodes);
          } else {
            Shiny.onInputChange(outputNodes, null);
          }

          var pipelineLinks = graph.getLinks();
          if (pipelineLinks.length > 0) {
            var outLinks = [];
            for (var iLink in pipelineLinks) {
              var link = pipelineLinks[iLink];
              outLinks.push({
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
            Shiny.onInputChange(outputLinks, outLinks);
          } else {
            Shiny.onInputChange(outputLinks, null);
          }

          // Return a root node for dfs search using igraph
          // jointjs internal dfs search works well for many graphs but not
          // those with 1->2; 3->2; 2->4 directed architecture
          elements = graph.getElements();
          if (elements.length === 0) {
            dfsRootID = null;
          } else {
            // look for nodes without an input port
            // get elements with no input port
            var noInput = elements.filter(function(x) { return !x.prop('hasInputPort'); });
            if (noInput.length > 0) {
              // find z value and choose lowest
              var z_noInput = noInput.map(function(x) { return x.prop('z'); });
              var dfsRootRef = z_noInput.indexOf(Math.max.apply(Math, z_noInput));
              // grab element id
              dfsRootID = noInput[dfsRootRef].id;
            } else {  // no obvious choice => take first node added
              dfsRootID = elements[0].id;
            }
          }
          Shiny.onInputChange(outputDFS, dfsRootID);

        });

      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});