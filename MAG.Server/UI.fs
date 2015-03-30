module MAG.Server.UI

open MAG
open MAG.Commands
open MAG.Server
open System.Runtime.Caching
open Chessie.ErrorHandling
open Chiron

let cache = new MemoryCache("UI Cache")

let inline unjson x = x |> Json.parse |> Json.deserialize
let inline mkjson x = x |> Json.serialize |> Json.format

let inline private get streamName foldName fold startState =
    async {
        let key = streamName + "||" + foldName
        match cache.Get key with
        | null ->
            let! newState = EventStore.refresh startState 0 fold streamName
            cache.Set(key, newState, CacheItemPolicy())
            return newState
        | item ->
            let cachedState, lastEvent = unbox item
            let! newState = EventStore.refresh cachedState (lastEvent + 1) fold streamName
            cache.Set(key, newState, CacheItemPolicy())
            return newState
    }

let getGame gid =
    get (sprintf "game-%s" gid) "gameFold" Fold.gameFold (ok Nothing)

let getPlayerView player gid =
    get (sprintf "game-%s" gid) (sprintf "playerViewFold||%A" player) (Fold.playerViewFold player) PlayerView.Nothing

let postStartGame players =
    let playerConfigs = unjson players
    let gid = System.Guid.NewGuid()
    let events =
        GameCommands.processCommand
            Nothing
            (StartGame (GameId gid, playerConfigs))    
    match events with
    | Ok (es, _) ->
        EventStore.persist -1 "gameEvent" es (sprintf "game-%A" gid)
        |> Async.map (fun _ -> mkjson gid)
    | Result.Fail error -> failwith <| sprintf "%A" error

let postPlayInitiative gid player cardStr =
    async {
        let card = unjson cardStr
        let! currentGameState, expected = getGame gid
        let events =
            currentGameState
            |> bind (fun g ->
                GameCommands.processCommand g (PlayInitiative (PlayerName player, card)))
        match events with
        | Ok (es, _) ->
            return!
                EventStore.persist expected "gameEvent" es (sprintf "game-%s" gid)
                |> Async.map (fun _ -> mkjson es)
        | Result.Fail error ->
            return failwith <| sprintf "%A" error
    }

let postPlayAttack gid player attackStr =
    async {
        let target, cards = unjson attackStr
        let! currentGameState, expected = getGame gid
        let events =
            currentGameState
            |> bind (fun g ->
                GameCommands.processCommand g (PlayAttack (PlayerName player, target, cards)))
        match events with
        | Ok (es, _) ->
            return!
                EventStore.persist expected "gameEvent" es (sprintf "game-%s" gid)
                |> Async.map (fun _ -> mkjson es)
        | Result.Fail error ->
            return failwith <| sprintf "%A" error
    }

let postCounter gid player cardsStr =
    async {
        let cards = unjson cardsStr
        let! currentGameState, expected = getGame gid
        let events =
            currentGameState
            |> bind (fun g ->
                GameCommands.processCommand g (Counter (PlayerName player, cards)))
        match events with
        | Ok (es, _) ->
            return!
                EventStore.persist expected "gameEvent" es (sprintf "game-%s" gid)
                |> Async.map (fun _ -> mkjson es)
        | Result.Fail error ->
            return failwith <| sprintf "%A" error
    }

let postMoveToStance gid player cardStr =
    async {
        let card = unjson cardStr
        let! currentGameState, expected = getGame gid
        let events =
            currentGameState
            |> bind (fun g ->
                GameCommands.processCommand g (MoveToStance (PlayerName player, card)))
        match events with
        | Ok (es, _) ->
            return!
                EventStore.persist expected "gameEvent" es (sprintf "game-%s" gid)
                |> Async.map (fun _ -> mkjson es)
        | Result.Fail error ->
            return failwith <| sprintf "%A" error
    }

let gameCreate decks = 
    let deckControls =
        decks
        |> List.mapi (fun i (DeckName d) -> sprintf "<option value=\"%d\">%s</option>" i d)
        |> String.concat "\n"
    let deckDictionary =
        decks
        |> List.mapi (fun i (DeckName d) -> sprintf """ "%d": "%s" """ i d)
        |> String.concat ", "
        |> sprintf "{ %s }"
    sprintf """<!DOCTYPE html>
<html>
    <head>
        <script src="/react.js"></script>
        <script src="/JSXTransformer.js"></script>
        <script src="/jquery.js"></script>
        <link rel="stylesheet" href="/bootstrap.css">
        <link rel="stylesheet" href="/bootstrap-theme.css">
        <script src="/bootstrap.js"></script> 
    </head>
    <body>
        <div id="main" class="container-fluid"></div>
        <script type="text/jsx">
            var DeckSelect = React.createClass({
                getInitialState: function() {
                    return {
                        decks: %s,
                        value: 0
                    };
                },
                handleChange: function(e) {
                    this.state.value = e.target.value;                    
                    this.forceUpdate();
                    this.props.callBack(this.state.decks[this.state.value]);
                },
                render: function() {
                    return (<select value={this.state.value} onChange={this.handleChange}>
                        %s
                    </select>);
                }
            });

            var PlayerConfig = React.createClass({
                getInitialState: function() {
                    return {name:"",deck:""};
                },
                deckChanged: function(deck) {
                    this.state.deck = deck;
                    this.forceUpdate();
                    this.props.callBack(this.state);
                },
                nameChanged: function(e) {
                    this.state.name = e.target.value;
                    this.forceUpdate();
                    this.props.callBack(this.state);
                },
                render: function() {
                    return (<div>
                        <input type="text" value={this.state.name} onChange={this.nameChanged} />
                        <DeckSelect callBack={this.deckChanged} />
                    </div>);
                }
            });

            var GameCreate = React.createClass({
                getInitialState: function() {
                    return {
                        player1: {name:"",deck:""},
                        player2: {name:"",deck:""},
                        result: null
                    };
                },
                update1: function (player) {
                    this.state.player1 = player;
                },
                update2: function (player) {
                    this.state.player2 = player;
                },
                submit: function(e) {
                    e.preventDefault();
                    this.post([this.state.player1, this.state.player2]);
                },
                post: function (data) {
                    console.log(data);
                    var baseUrl = '/api/game';
                    $.ajax({
                        url: baseUrl,
                        dataType: 'json',
                        type: 'POST',
                        data: JSON.stringify(data),
                        success: function (d) {
                            this.state.result = d;
                            this.forceUpdate();
                        }.bind(this),
                        error: function (xhr, status, error) {
                            console.error(baseUrl, status, error.toString());
                        }.bind(this)
                    });
                },
                render: function() {
                    var gameForm = (<div>
                        <form onSubmit={this.submit}>
                            <h3>Player 1</h3>
                            <PlayerConfig callBack={ this.update1 } />
                            <h3>Player 2</h3>
                            <PlayerConfig callBack={ this.update2 } />
                            <input type="submit" action="POST" />
                        </form>
                        </div>);
                    var linksToGame = (<div>
                        <a href={ "/play/" + this.state.player1.name + "/" + this.state.result }>{ this.state.player1.name }</a>
                        <a href={ "/play/" + this.state.player2.name + "/" + this.state.result }>{ this.state.player2.name }</a>
                    </div>);
                    if(this.state.result === null) {
                        return gameForm;
                    } else {
                        return linksToGame;
                    }
                }
            });

            React.render(
                <GameCreate />,
                document.getElementById('main')
            );
        </script>
    </body>
</html>""" deckDictionary deckControls

let playerViewHtml player gid = 
    sprintf """<!DOCTYPE html>
<html>
    <head>
        <script src="/react.js"></script>
        <script src="/JSXTransformer.js"></script>
        <script src="/jquery.js"></script>
        <link rel="stylesheet" href="/bootstrap.css">
        <link rel="stylesheet" href="/bootstrap-theme.css">
        <script src="/bootstrap.js"></script> 
    </head>
    <body>
        <div id="main" class="container-fluid"></div>
        <script type="text/jsx">
            var player = '%s';
            var gid = '%s';

            var Card = React.createClass({
                render: function () {
                    if (this.props.card.type === 'basic') {
                        return (
                                <li>{ this.props.card.suit }-{ this.props.card.value }</li>
                        );
                    } else if (this.props.card.type === 'knockdown') {
                        return (<li>Knockdown!</li>);
                    } else if (this.props.card.type === 'mega') {
                        return (<li>{ this.props.card.name } ({ this.props.card.speed }/{ this.props.card.damage })</li>);
                    } else if (this.props.card.type === 'combo') {
                        return (<li>Combo! ({ this.props.card.speed }/{ this.props.card.extra })</li>);
                    }
                }
            });

            var CardList = React.createClass({
                render: function () {
                    var cards = this.props.cards.map(
                                function (card, i) { return (<Card card={card} key={i} />); }                    
                            );
                    return (<ul>{ cards }</ul>);
                }
            });

            var You = React.createClass({
                render: function() {
                    return (
                        <div>
                            <h3>You are: { this.props.you.name } - Life { this.props.you.life }</h3>
                            <h3>Hand:</h3>
                            <CardList cards={ this.props.you.hand } />
                            <h3>Stance:</h3>
                            <CardList cards={ this.props.you.stance } />
                            <h3>Discards:</h3>
                            <CardList cards={ this.props.you.discards } />
                        </div>);
                }
            });

            var Them = React.createClass({
                render: function () {
                    return (
                        <li>
                            <h3>They are: { this.props.them.name } - Life { this.props.them.life }</h3>
                            <h3>Stance:</h3>
                            <CardList cards={ this.props.them.stance } />
                            <h3>Discards:</h3>
                            <CardList cards={ this.props.them.discards } />
                        </li>);
                }
            });

            var ThemList = React.createClass({
                render: function () {
                    var them = this.props.them.map(
                                function (them, i) { return (<Them them={them} key={i} />); }                    
                            );
                    return (<ul>{ them }</ul>);
                }
            });

            var HandSelect = React.createClass({
                getInitialState: function() { return { value: null }; },
                handleChange: function(e) {
                    var card;
                    if (e.target.value == -1) {
                        card = null;
                    } else {
                        card = this.props.cards[e.target.value];
                    }
                    this.setState({ value: e.target.value });
                    this.props.callBack(card);
                },
                render: function () {
                    var cards = this.props.cards.map(
                            function (card, i) { return (<option value={ i }><Card card={card} /></option>); }
                        );
                    return (
                        <select value={this.state.value} onChange={ this.handleChange }>
                            {cards}
                            <option value="-1">None</option>
                        </select>
                    );
                }
            });

            var StanceSelect = React.createClass({
                getInitialState: function() { return { value: [] }; },
                handleChange: function(e) {
                    if (e.target.value === "") {
                        this.state.value.length = 0;
                    } else {
                        this.state.value.push(e.target.value);
                    }
                    var cards = this.props.cards;
                    var selected = this.state.value.map(function (i) { return cards[i]; });
                    this.props.callBack(selected);
                },
                render: function () {
                    var cards = this.props.cards.map(
                            function (card, i) { 
                                return (<option key={ i } value={ i }><Card card={card} /></option>);
                            }
                        );
                    return (
                        <select multiple={true} value={this.state.value} onChange={ this.handleChange }>{cards}</select>
                    );
                }
            });

            var TargetSelect = React.createClass({
                getInitialState: function() { return { value: "-1" }; },
                handleChange: function(e) {
                    var target;
                    if (e.target.value === "-1") {
                        target = null;
                    } else {
                        target = this.props.them[e.target.value];
                    }
                    this.props.callBack(target);
                    this.state.value = e.target.value;
                },
                render: function () {
                    var targets = this.props.them.map(
                            function (t, i) { return (<option value={ i }>{ t }</option>); }
                        );
                    return (
                        <select value={this.state.value} onChange={ this.handleChange }>
                            {targets}
                            <option value="-1">None</option>
                        </select>
                    );
                }
            });

            var CommandBox = React.createClass({
                getInitialState: function() {
                    return {
                        hand: null,
                        stance: [],
                        target: null
                    };
                },
                stanceUpdated: function(stance) {
                    this.state.stance = stance;
                },
                handUpdated: function(hand) {
                    this.state.hand = hand;
                },
                targetUpdated: function(target) {
                    this.state.target = target;
                },
                post: function (action, data) {
                    console.log(data);
                    var baseUrl = '/api/player/' + player + '/' + gid + '/';
                    $.ajax({
                        url: baseUrl + action,
                        dataType: 'json',
                        type: 'POST',
                        data: JSON.stringify(data),
                        success: function (d) {
                            return;
                        },
                        error: function (xhr, status, error) {
                            console.error(baseUrl, status, error.toString());
                        }.bind(this)
                    });
                },
                handleSubmit: function (action, e) {
                    e.preventDefault();
                    if (action === 'initiative') {
                        var card = this.state.hand;
                        this.post(action, card);
                    } else if (action === 'stance') {
                        var card = this.state.hand;
                        this.post(action, card);
                    } else if (action === 'counter') {
                        var hand = this.state.hand;
                        var cards = this.state.stance.slice(0);
                        if (hand != null) { cards.push(hand); }
                        this.post(action, cards);
                    } else if (action === 'attack') {
                        var hand = this.state.hand;
                        var cards = this.state.stance.slice(0);
                        if (hand != null) { cards.push(hand); }
                        this.post(action, [this.state.target, cards]);
                    }
                },
                render: function () {
                    if (this.props.waiting === 'You to play initiative') {
                        var action = 'initiative';
                        return (<form className="commentForm" onSubmit={this.handleSubmit.bind(null, action)}>
                                    <HandSelect cards={this.props.hand} callBack={this.handUpdated} />
                                    <input type="submit" value="Post" />
                                </form>);
                    } else if (this.props.waiting === 'You to move a card to stance') {
                        var action = 'stance';
                        return (<form className="commentForm" onSubmit={this.handleSubmit.bind(null, action)}>
                                    <HandSelect cards={this.props.hand} callBack={this.handUpdated} />
                                    <input type="submit" value="Post" />
                                </form>);
                    } else if (this.props.waiting === 'You to counter') {
                        var action = 'counter';
                        return (<form className="commentForm" onSubmit={this.handleSubmit.bind(null, action)}>
                                    <HandSelect cards={this.props.hand} callBack={this.handUpdated} />
                                    <StanceSelect cards={this.props.stance} callBack={this.stanceUpdated} />
                                    <input type="submit" value="Post" />
                                </form>);
                    } else if (this.props.waiting === 'You to attack') {
                        var action = 'attack';
                        return (<form className="commentForm" onSubmit={this.handleSubmit.bind(null, action)}>
                                    <TargetSelect them={this.props.them} callBack={this.targetUpdated} />
                                    <HandSelect cards={this.props.hand} callBack={this.handUpdated} />
                                    <StanceSelect cards={this.props.stance} callBack={this.stanceUpdated} />
                                    <input type="submit" value="Post" />
                                </form>);
                    }
                    return (<div />);
                }
            });

            var WaitingFor = React.createClass({
                render: function () {
                    if (this.props.waitingFor.waiting === "You to counter") {
                        return (<span>You to counter {this.props.waitingFor.attacker}'s attack of <CardList cards={this.props.waitingFor.cards} /></span>);
                    } else {
                        return (<span>{this.props.waitingFor.waiting}</span>);
                    }
                }
            });

            var GameView = React.createClass({
                getInitialState: function () {
                    return { download: [
                        {
                            data: {
                                you: {
                                    hand: [],
                                    name: "",
                                    stance: [],
                                    life: 0,
                                    discards: []
                                },
                                them: [],
                                waitingFor: { waiting: "" }
                            }
                        }, 0]};
                },
                loadData: function() {
                    $.ajax({
                        url: '/api/player/' + player + '/' + gid,
                        dataType: 'json',
                        success: function (data) {
                            this.setState({ download : data });
                        }.bind(this),
                        error: function (xhr, status, err) {
                            console.error("oh noes", status, err.toString());
                        }.bind(this)
                    });
                },
                componentDidMount: function () {
                    this.loadData();
                    setInterval(this.loadData, 5000);
                },
                render: function () {
                    console.log(this.state.download[0].data.waitingFor);
                    var waiting = this.state.download[0].data.waitingFor.waiting;
                    var you = this.state.download[0].data.you;
                    var theirNames = this.state.download[0].data.them.map(function (t) { return t.name; });
                    return (
                        <div>
                            <h3>The game is waiting for: <WaitingFor waitingFor={this.state.download[0].data.waitingFor} /> during { this.state.download[0].data.turn }'s turn</h3>
                            <You you={ you } />
                            <ThemList them={ this.state.download[0].data.them } />
                            <CommandBox waiting={ waiting } hand={ you.hand } stance={ you.stance } them={ theirNames } />
                        </div>);
                }
            });

            React.render(
                <GameView />,
                document.getElementById('main')
            );
        </script>
    </body>
</html>""" player gid