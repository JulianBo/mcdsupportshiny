//https://stackoverflow.com/questions/40415471/sliderinput-max-min-text-labels?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa

$(document).ready(function() {
  /**
    Custom slider labels
  **/

    // Convert numbers of min to max to "lower" and "higher"
    function returnLabelsNormal(value) {

      if (value === 0){ // enter your lowest slider value here
        return "nicht wichtig";
      }else if (value == 100){// enter your highest slider value here
        return "sehr wichtig";
      } else
      return value;
    }

   function returnLabelsAnlagengroesse(value) {

      if (value == -100){ // enter your lowest slider value here
        return "viele kleine Anlagen";
      }else if (value == 100){// enter your highest slider value here
        return "wenige große Anlagen";
      } if (value === 0){// enter your highest slider value here
        return "egal";
      } else
      return value;
    }

  function returnLabelsProzent(value) {

      if (value == 100){// enter your highest slider value here
        return "100% (alles einbeziehen)";
      } if (value === 0){// enter your highest slider value here
        return "0% (nicht einbeziehen)";
      } else
      return value;
    }


    var someID = $(".js-range-slider").ionRangeSlider({ // enter your shiny slider ID here (CSS-Selector?): .js-range-slider #bins
          prettify: returnLabelsNormal
          // ,
          // force_edges: true,
          // grid: false
        });
    });
