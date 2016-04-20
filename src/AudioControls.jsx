import React, {PropTypes} from "react";
import ReactDOM from "react-dom";

import {PlayButton, Progress} from 'react-soundplayer/components';
import Audio from "./Audio";



const SC = React.createClass({
  getInitialState() {
    return {
      playing: false,
      val: 0
    };
  },
  componentWillMount() {
    Audio.initializeAudioSoundCloud();
    console.log(Audio.scPlayer);
    this.scPlayer = Audio.scPlayer;
    this.update();
  },
  componentWillUpdate(nprops, nstate) {
    if (nstate.playing !== this.state.playing){
      if (nstate.playing) {
        this.update();
        this.scPlayer.play();
      } else {
        window.clearTimeout(this.intervalId);
        this.scPlayer.pause();
      }
    }
  },
  togglePlay() {
    this.setState((prev) => {return {playing: !prev.playing};});
  },
  update() {
    this.intervalId = window.setInterval(() => {
      let val = this.scPlayer.audio.currentTime/(this.scPlayer.duration / 100);
      this.setState({val: val});
    }, 1000);
  },
  componentWillUnmount() {
    window.clearTimeout(this.intervalId);
  },
  seek(e) {
    console.log(e);
    console.log(e.offsetX);
  },
  render: function() {
    let buttonVal = this.state.playing ? "[⏸]" : "[▶]";
    return (
      <div className="scPlayer">
        <span onClick={this.togglePlay}> {buttonVal}</span>
        {/* <Progress value={this.state.pos} onSeekTrack={onSeekTrack}/> */}
        <Progress innerStyle={{}}
                  soundCloudAudio={this.scPlayer}
                  value={this.state.val}/>
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
      state: "sc"
    };
  },
  onChange(e) {
    this.setState({
      state: e.currentTarget.value
    });
  },
  render() {

    let component = React.createElement(this.state.state === "sc" ? SC : UserMedia);

    let onChanged = function(e) {
      console.log(e);
    };

    return (
      <div className="audioControls">
        <div>
          <br/>
          Soundcloud: <input type="radio"
                 checked={this.state.state === "sc"}
                 value="sc"
                 onChange={this.onChange}/>
          <br/>
          Microphone: <input type="radio"
                 checked={this.state.state === "usermedia"}
                 value="usermedia"
                 onChange={this.onChange}/>
        </div>
        {component}
      </div>
    );
  }
});
