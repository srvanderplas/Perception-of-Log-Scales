// !preview r2d3 data = tibble(x = seq(1, 25, .5), y = exp((x-15)/30), ydotAll = exp(((x-15)/30)+rnorm(49, 0, 0.05))) %>% mutate(ydot = ifelse(x > 15, NA, ydotAll))

// 1. ACCESS DATA

const xAccessor = d => d.x
const yAccessor = d => d.y
const yDotAllAccessor = d => d.ydotAll
const yDotAccessor = d => d.ydot

// 2. CREATE CHART DIMENSIONS -------------------------------------------

  let dimensions = {
    width: width*0.9,
    height: height,
    margin:{
      top: 15,
      right: 15,
      bottom: 40,
      left: 60,
    },
  }
  
  //compute size of bounds (contains all of the data elements: in this case, our line)
  dimensions.boundedWidth  = dimensions.width - dimensions.margin.left - dimensions.margin.right
  dimensions.boundedHeight = dimensions.height - dimensions.margin.top - dimensions.margin.bottom
  

// 3. DRAW CANVAS -------------------------------------------------------

const wrapper = svg.selectAll('rect')
    .attr("id", "wrapper")
    .attr("width", dimensions.width)
    .attr("height", dimensions.height)
    
const bounds = wrapper.append("g")
    .style("transform", `translate(${dimensions.margin.left}px, ${dimensions.margin.top}px)`)

//const bounds = svg.append('g').translate([dimensions.margin.left, dimensions.margin.top])  


  // maybe this is already here???
  
  /*
  const wrapper = d3.select("#wrapper")
    .append("svg")
    .attr("width", dimensions.width)
    .attr("height", dimensions.height)
    
  const bounds = wrapper.append("g")
    .style("transform", `translate(${dimensions.margin.left}px, ${dimensions.margin.top}px)`)
  
  */


// 4. CREATE SCALES ------------------------------------------------------

const yScale = d3.scaleLinear()
  .domain(d3.extent(data, yAccessor))
  .range([dimensions.boundedHeight, 0])
  
const yDotScale = d3.scaleLinear()
  .domain(d3.extent(data, yDotAllAccessor))
  .range([dimensions.boundedHeight, 0])
    
const xScale = d3.scaleTime()
  .domain(d3.extent(data, xAccessor))
  .range([0, dimensions.boundedWidth])


// 5. DRAW DATA -----------------------------------------------------------

const lineGenerator = d3.line()
      .x(d => xScale(xAccessor(d)))
      .y(d => yScale(yAccessor(d)))

bounds
  .data(data)
  .enter().append('path')
  .attr("d", lineGenerator(data))
  .attr("fill", "none") /*svg default to black filled*/ 
  .attr("stroke", "steelblue") /*change to orangish line stroke*/
  .attr("stroke-width", 2)
  
bounds
    .data(data) /* joining selected elements with our array of data points */
    .enter().append("circle")
    .attr("cx", d => xScale(xAccessor(d)))
    .attr("cy", d => yDotScale(yDotAccessor(d)))
    .attr("r", 3)

/*
svg.selectAll('rect')
  .data(data)
  .enter().append('path')
  .attr("d", lineGenerator(data))
  .attr("fill", "none") /*svg default to black filled*/ 
//  .attr("stroke", "steelblue") /*change to orangish line stroke*/
//  .attr("stroke-width", 2)
  
  
// 6. DRAW PERIPHERALS (AXIS) ---------------------------------------------
  
// x axis  
const xAxisGenerator = d3.axisBottom()
  .scale(xScale)
  
const xAxis = svg.append("g")
  .call(xAxisGenerator)
    .style("transform", `translateY(${dimensions.boundedHeight}px)`)
      
const xAxisLabel = xAxis.append("text")
  .attr("x", dimensions.boundedWidth / 2)
  .attr("y", dimensions.margin.bottom - 10)
  .attr("fill", "black")
  .style("font-size", "1.4em")
  .html("x")
  
// y axis  
const yAxisGenerator = d3.axisLeft()
  .scale(yDotScale)
  .ticks(4)
      
const yAxis = svg.append("g")
  .call(yAxisGenerator)
  
const yAxisLabel = yAxis.append("text")
  .attr("x", -dimensions.boundedHeight/2)
  .attr("y", -dimensions.margin.left + 10)
  .attr("fill", "black")
  .style("font-size", "1.4em")
  .text("y")
  .style("transform", "rotate(-90deg)")
  .style("text-anchor", "middle")
  
  
  