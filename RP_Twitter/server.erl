-module(server).
-import(client, [registerUser/3, simulate/3]).
-import(lists, [nth/2, append/1]).
-export([
    start/2,
    createUsers/4,
    createEmptyRecordList/3,
    simulator/4,
    takeOnline/2,
    replaceNthIdx/3,
    setUserAsOffline/2
]).
-record(user, {
    id, followers = [], following = [], tweet = [], feed = [], mentions = [], status = false
}).

start(NumUsers, NumTweets) ->
    io:format("Input number of users: ~p ~n  Input number of tweets: ~p ~n", [NumUsers, NumTweets]),
    UserList = gen(1, NumUsers, self()),
    io:format("Generated user list for the input: ~p ~n", [UserList]),
    Records = createEmptyRecordList(1, NumUsers, []),
    HashTags = #{"HashTagOne" => [], "HashTagTwo" => [], "HashTagThree" => [], "HashTagFour" => [], "HashTagFive" => []},
    spawn(client, simulate, [UserList, self(), 1]),
    simulator(Records, HashTags, NumTweets, 0).

gen(StartIdx, EndIdx, ProcessId) ->
    createUsers(StartIdx, EndIdx, [], ProcessId).

simulator(Records, HashTags, NumTweets, TotalTweets) ->
    receive
        {addFollowers, FollowUid, Uid} ->
            P = nth(Uid, Records),
            FollowerList = P#user.followers,
            DoesUserExist = lists:member(FollowUid, FollowerList),
            case DoesUserExist == false of
                true ->
                    List1 = appendToFollowerList(Uid, FollowUid, Records),
                    List2 = appendToFollowingList(Uid, FollowUid, List1),
                    simulator(List2, HashTags, NumTweets, TotalTweets);
                false ->
                    simulator(Records, HashTags, NumTweets, TotalTweets)
            end;
        {makeOnline, Uid, Pid} ->
            io:format("User ~p is in online and has PID ~p ~n", [Uid, Pid]),
            List1 = takeOnline(Records, Uid),
            simulator(List1, HashTags, NumTweets, TotalTweets);
        {setFollowers, Uid, FollowerList} ->
            io:format("Current list of followers ~p assigned to ~p ~n", [FollowerList, Uid]),
            List1 = addFollowers(Uid, FollowerList, Records),
            Length = length(FollowerList),
            List2 = addToFollowing(Uid, FollowerList, List1, 1, Length),
            simulator(List2, HashTags, NumTweets, TotalTweets);
        {go_offline, Uid} ->
            List1 = setUserAsOffline(Records, Uid),
            simulator(List1, HashTags, NumTweets, TotalTweets);
        {go_online, Uid} ->
            List1 = setUserAsOnline(Records, Uid),
            simulator(List1, HashTags, NumTweets, TotalTweets);
        {postTweet, Uid, Tweet} ->
            List1 = postTweet(Records, Uid, Tweet),
            P = nth(Uid, List1),
            FollowerList = P#user.followers,
            Length = length(FollowerList),
            SumTotalTweets=Length+TotalTweets,
            List2 = distributeTweet(List1, Uid, Tweet, FollowerList, 1, Length),
            ContainsHashTag = string:chr(Tweet, $#),
            ContainsMention = string:chr(Tweet, $@),
            case SumTotalTweets<NumTweets of
                true->
                    case ContainsHashTag =/= 0 of
                        true ->
                            Start = ContainsHashTag + 1,
                            End = ContainsHashTag + 4,
                            Tag = string:substr(Tweet, Start, End),
                            TagList = maps:get(Tag, HashTags),
                            TagList2 = lists:append(TagList, [Tweet]),
                            Map2 = maps:put(Tag, TagList2, HashTags),
                            case ContainsMention =/= 0 of
                                true ->
                                    Start1 = ContainsMention + 1,
                                    End1 = string:chr(Tweet, $\s),
                                    MentionedUser = string:substr(Tweet, Start1, End1 - 1),
                                    UserMentioned = integer_to_list(MentionedUser),
                                    List3 = tweetWithMention(List2, UserMentioned, Tweet),
                                    simulator(List3, Map2, NumTweets, SumTotalTweets);
                                false ->
                                    simulator(List2, Map2, NumTweets, SumTotalTweets)
                            end;
                        false ->
                            case ContainsMention =/= 0 of
                                true ->
                                    Start2 = ContainsMention + 1,
                                    End2 = string:chr(Tweet, $\s),
                                    MentionedUser = string:substr(Tweet, Start2, End2 - 1),
                                    UserMentioned = integer_to_list(MentionedUser),
                                    List3 = tweetWithMention(List2, UserMentioned, Tweet),
                                    simulator(List3, HashTags, NumTweets, SumTotalTweets);
                                false ->
                                    simulator(List2, HashTags, NumTweets, SumTotalTweets)
                            end 
                    end;
                false->
                    io:format("Posted ~p tweets, reached termination condition, Bye! ~n", [SumTotalTweets])
            end;

        {display_hashtags, Hash} ->
            TweetList = maps:get(Hash, HashTags),
            io:format("Displaying ~p hashtags: ~p ~n", [Hash, TweetList]),
            simulator(Records, HashTags, NumTweets, TotalTweets);
        {display_mentions, Uid} ->
            MentionList = showMentions(Uid, Records),
            io:format("Displaying mentions for user ~p: ~p ~n", [Uid, MentionList]),
            simulator(Records, HashTags, NumTweets, TotalTweets);
        {get_feed, Uid} ->
            Feed = showFeed(Uid, Records),
            io:format("Displaying feed for user ~p: ~p ~n", [Uid, Feed]),
            simulator(Records, HashTags, NumTweets, TotalTweets);
        {retweet, Uid} ->
            P = nth(Uid, Records),
            FeedList = P#user.feed,
            FeedListLength = length(FeedList),
            FollowerList = P#user.followers,
            Length = length(FollowerList),
            SumTotalTweets=Length+TotalTweets,
            case SumTotalTweets<NumTweets of
                true->
                    case FeedListLength =/= 0 of
                        true ->
                            RandTweetIdx = rand:uniform(FeedListLength),
                            RandTweet = nth(RandTweetIdx, FeedList),
                            List1 = reTweeting(Uid, Records, RandTweet),
                            List2 = distributeTweet(List1, Uid, RandTweet, FollowerList, 1, Length),
                            ContainsHashTag = string:chr(RandTweet, $#),
                            case ContainsHashTag =/= 0 of
                                true ->
                                    Start = ContainsHashTag + 1,
                                    End = ContainsHashTag + 4,
                                    Tag = string:substr(RandTweet, Start, End),
                                    TagList = maps:get(Tag, HashTags),
                                    TagList2 = lists:append(TagList, [RandTweet]),
                                    Map2 = maps:put(Tag, TagList2, HashTags),
                                    simulator(List2, Map2, NumTweets, SumTotalTweets);
                                false ->
                                    simulator(List2, HashTags, NumTweets, SumTotalTweets)
                            end;
                        false ->
                            io:format("Retweet count reached, nothing in feed~n", []),
                            simulator(Records, HashTags, NumTweets, SumTotalTweets)
                    end;
                false->
                    io:format("Terminating from retweets as max limit ~p of tweets reached ~n", [SumTotalTweets])
            end
    end.

tweetWithMention(List, Uid, Tweet) ->
    P = nth(Uid, List),
    MentionList = P#user.mentions,
    P1 = P#user{mentions = lists:append(MentionList, [Tweet])},
    replaceNthIdx(List, Uid, P1).

reTweeting(Uid, List, RandTweet) ->
    P = nth(Uid, List),
    TweetList = P#user.tweet,
    P1 = P#user{tweet = lists:append(TweetList, [RandTweet])},
    replaceNthIdx(List, Uid, P1).

appendToFollowingList(Uid, FollowUid, List) ->
    P = nth(FollowUid, List),
    FollowingList = P#user.following,
    P1 = P#user{following = lists:append(FollowingList, [Uid])},
    replaceNthIdx(List, FollowUid, P1).

appendToFollowerList(Uid, FollowUid, List) ->
    P = nth(Uid, List),
    FollowerList = P#user.followers,
    P1 = P#user{followers = lists:append(FollowerList, [FollowUid])},
    replaceNthIdx(List, Uid, P1).

showFeed(Uid, List) ->
    P = nth(Uid, List),
    P#user.feed.

showMentions(Uid, List) ->
    P = nth(Uid, List),
    P#user.mentions.

distributeTweet(List, Uid, Tweet, FollowerList, S, Length) ->
    case S =< Length of
        true ->
            Curr = nth(S, FollowerList),
            P = nth(Curr, List),
            FeedList = P#user.feed,
            P1 = P#user{feed = lists:append(FeedList, [Tweet])},
            List2 = replaceNthIdx(List, Curr, P1),
            distributeTweet(List2, Uid, Tweet, FollowerList, S + 1, Length);
        false ->
            List
    end.

postTweet(List, Uid, Tweet) ->
    P = nth(Uid, List),
    TweetList = P#user.tweet,
    io:format("Posting tweet for user ~p, ~p", [Uid, Tweet]),
    P1 = P#user{tweet = lists:append(TweetList, [Tweet])},
    replaceNthIdx(List, Uid, P1).

setUserAsOffline(List, Uid) ->
    P = lists:nth(Uid, List),
    Bool = P#user.status,
    case Bool == true of
        true ->
            P1 = P#user{status = false},
            replaceNthIdx(List, Uid, P1);
        false ->
            io:format("User has signed out and is offline, sign in again. ~n", []),
            List
    end.

setUserAsOnline(List, Uid) ->
    P = lists:nth(Uid, List),
    Bool = P#user.status,
    case Bool == false of
        true ->
            P1 = P#user{status = true},
            replaceNthIdx(List, Uid, P1);
        false ->
            io:format("User is already online, no need to sign in again. ~n", []),
            List
    end.

addToFollowing(Uid, FollowerList, List, S, Length) ->
    case S =< Length of
        true ->
            Curr = nth(S, FollowerList),
            P = nth(Curr, List),
            TempList = P#user.following,
            P1 = P#user{following = lists:append(TempList, [Uid])},
            List2 = replaceNthIdx(List, Curr, P1),
            addToFollowing(Uid, FollowerList, List2, S + 1, Length);
        false ->
            List
    end.

addFollowers(Uid, FollowerList, List) ->
    P = nth(Uid, List),
    P1 = P#user{followers = FollowerList},
    replaceNthIdx(List, Uid, P1).

takeOnline(List, Uid) ->
    P = nth(Uid, List),
    P1 = P#user{status = true},
    replaceNthIdx(List, Uid, P1).

replaceNthIdx(List, Index, NewValue) ->
    {L1, [_ | L2]} = lists:split(Index - 1, List),
    L1 ++ [NewValue | L2].

createUsers(StartIdx, EndIdx, List, ProcessId) ->
    case StartIdx =< EndIdx of
        true ->
            createUsers(
                StartIdx + 1, EndIdx, lists:append([List, [spawn(client, registerUser, [StartIdx, ProcessId, EndIdx])]]), ProcessId
            );
        false ->
            io:format("Required users are created. ~n"),
            List
    end.

createEmptyRecordList(StartIdx, EndIdx, List) ->
    case StartIdx =< EndIdx of
        true ->
            createEmptyRecordList(
                StartIdx + 1,
                EndIdx,
                lists:append([
                    List,
                    [
                        #user{
                            id = StartIdx,
                            followers = [],
                            following = [],
                            tweet = [],
                            feed = [],
                            mentions = [],
                            status = false
                        }
                    ]
                ])
            );
        false ->
            List
    end.
