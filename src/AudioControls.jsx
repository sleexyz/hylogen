import React, {PropTypes} from "react";
import ReactDOM from "react-dom";

import {PlayButton, Progress} from 'react-soundplayer/components';
import Audio from "./Audio";



const SC = React.createClass({
  propTypes: {
    initPlaying: PropTypes.bool
  },
  getDefaultProps() {
    return {
      initPlaying: false
    };
  },
  getInitialState() {
    console.log(localStorage.getItem("scurl"));
    let defaulturl = localStorage.getItem("scurl")
                  || "https://soundcloud.com/tennysonmusic/xyz"
                  || "https://soundcloud.com/tennysonmusic/angus-julia-stone-for-you"
                  || "https://soundcloud.com/herzeloyde/deception"
                  || "https://soundcloud.com/aslamin/strannoe-chuvstvo";
    return {
      playing: this.props.initPlaying,
      val: 0,
      url: defaulturl
    };
  },
  startPlaying() {
    this.startUpdating();
    this.scPlayer.play();
  },
  stopPlaying() {
    console.log("clearing ", this.intervalId);
    window.clearTimeout(this.intervalId);
    this.scPlayer.pause();
  },
  componentWillMount() {
    console.log(this.props.initPlaying);
    Audio.initializeAudioSoundCloud(this.state.url, this.props.initPlaying);
    console.log(this.state.url);
    console.log(Audio.scPlayer);
    this.scPlayer = Audio.scPlayer;
  },
  componentDidMount() {
    if (this.props.initPlaying) {
      this.startUpdating();
    }
  },
  componentWillUpdate(nprops, nstate) {
    if (nstate.playing !== this.state.playing){
      if (nstate.playing) {
        this.startPlaying();
      } else {
        this.stopPlaying();
      }
    }
  },
  togglePlay() {
    this.setState((prev) => {return {playing: !prev.playing};});
  },
  update() {
    let val = this.scPlayer.audio.currentTime/(this.scPlayer.duration / 100);
    this.setState({val: val});
  },
  startUpdating() {
    this.intervalId = window.setInterval(this.update, 100);
    console.log("made new interval:", this.intervalId);
  },
  componentWillUnmount() {
    console.log("clearing: ", this.intervalId);
    window.clearTimeout(this.intervalId);
  },
  seek(e) {
    console.log(e);
    console.log(e.offsetX);
  },
  onUrlChange(e) {
    this.setState({url: e.currentTarget.value});
  },
  onSubmit(e) {
    e.preventDefault();
    localStorage.setItem("scurl", this.state.url);
    this.scPlayer.resolve(this.state.url, function(track) {
      console.log(track);
      this.setState({playing: false}, function() {
        console.log("callback");
        this.setState({playing: true});
      }.bind(this));
    }.bind(this));
  },
  render: function() {
    let buttonVal = this.state.playing ? "[pause]" : "[play]";
    return (
        <div className="scPlayer">
        <div className="row"
                      onClick={this.update} >
            <button onClick={this.togglePlay}> {buttonVal}</button>
            <Progress innerStyle={{}}
                      soundCloudAudio={this.scPlayer}
                      value={this.state.val}/>
          </div>
          <div className="row">
            <form onSubmit={this.onSubmit}>
              <input value={this.state.url}
                     style={{width: "40vw"}}
                    onChange={this.onUrlChange}/>
            </form>
          </div>
        </div>
    );
  }
});

const UserMedia = React.createClass({
  componentWillMount() {
    Audio.initializeAudioUserMedia();
  },
  render: function() {
    return (
      <div>
      </div>
    );
  }
});

export default React.createClass({
  getInitialState: function() {
    return {
      state: "sc",
      initPlaying: false
    };
  },
  onChange(e) {
    this.setState({
      state: e.currentTarget.value,
      initPlaying: true
    });
  },
  render() {

    let component = this.state.state === "sc"
                  ? <SC initPlaying={this.state.initPlaying}/>
                  : <UserMedia/>;

    let onChanged = function(e) {
      console.log(e);
    };

    return (
      <div className="audioControls">
        <div className="inputPicker">
          <br/>
          soundcloud: <input type="radio"
                 checked={this.state.state === "sc"}
                 value="sc"
                 onChange={this.onChange}/>
          <br/>
          microphone: <input type="radio"
                 checked={this.state.state === "usermedia"}
                 value="usermedia"
                 onChange={this.onChange}/>
        </div>
        {component}
      </div>
    );
  }
});
