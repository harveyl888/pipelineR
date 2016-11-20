

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
        div_all.id = id;
        div_stencil.id = id + '-stencil';
        div_stencil.classList.add('div_stencil');
        div_paper.id = id + '-paper';
        div_paper.classList.add('div_paper');
        div_all.appendChild(div_stencil);
        div_all.appendChild(div_paper);
        el.appendChild(div_all);

      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
