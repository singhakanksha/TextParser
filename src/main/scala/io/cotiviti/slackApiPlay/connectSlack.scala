package io.cotiviti.slackApiPlay

import java.io.IOException
import java.util.Optional

import com.ullink.slack.simpleslackapi.events.SlackMessagePosted
import com.ullink.slack.simpleslackapi.listeners.SlackMessagePostedListener
import com.ullink.slack.simpleslackapi.{SlackUser, SlackChannel, SlackSession}
import com.ullink.slack.simpleslackapi.impl.SlackSessionFactory
import org.apache.commons.lang3.StringUtils
import org.json4s.scalap.{Failure, Success}
import org.scalacheck.Gen

import scala.util.Try


object connectSlack extends App{
  val token = ""
  val slackSession = SlackSessionFactory.createWebSocketSlackSession(token)

  try {
    slackSession.connect()
  } catch {
    case e: Exception => throw new RuntimeException("could not connect !!!! :(")
  }

  def start(): Unit ={
//    sendMessageToAChannel(slackSession)
//    sendDirectMessageToAUser(slackSession)
   // sendDirectMessageToAUserTheHardWay(slackSession)
    replyAppropriately(slackSession)
  }

  def sendMessageToAChannel( session: SlackSession,channelName: String)
  {

    //get a channel
    val channel = session.findChannelByName(channelName);

    session.sendMessage(channel, "Hey there");
  }

  def sendDirectMessageToAUser(session: SlackSession, user1:String)
  {

    //get a user
    val user = session.findUserByUserName(user1);

    session.sendMessageToUser(user, "Hi, how are you", null);

  }

  def sendDirectMessageToAUserTheHardWay( session:SlackSession)
  {

    //get a user
    val user = session.findUserByUserName("akanksha");

    //get its direct message channel
    val reply = session.openDirectMessageChannel(user);

    //get the channel
    val channel = reply.getReply().getSlackChannel();

    //send the message to this channel
    session.sendMessage(channel, "Hi, how are you", null);
  }



  def replyAppropriately(session:SlackSession): Unit ={

    val billList = List("it is skill associated with mathematics with something of pertaining to the ability to do data processing, being smart",
    "it is the virtue of having the right necessary understanding or knowledge at the right moment",
    "It is not that I’m so smart. But I stay with the questions much longer.",
    "Where there is no vision, the people perish",
    "Treat your customers like they own you. Because they do",
    "60")
    def arbStringGen: Gen[String] = for {
      bil <- Gen.oneOf(billList)
    } yield bil


    session.addMessagePostedListener(new SlackMessagePostedListener {
      override def onEvent(event: SlackMessagePosted, session: SlackSession): Unit = {
        if(!event.getSender().getId().equals(session.sessionPersona().getId())){
          if(StringUtils.startsWithIgnoreCase(event.getMessageContent(),"<@" + session.sessionPersona().getId() + ">")){
            // the message is addressed to this bot
            var message = event.getMessageContent().substring(session.sessionPersona().getId().length()+3);
            if(message.startsWith(":")){
              message = message.substring(1);
            }
            message = message.trim();
            if(message == "blueJeans url"){
              session.sendMessage(event.getChannel(), """https://bluejeans.com/939552048/""", null);
            }else {
              session.sendMessage(event.getChannel(), arbStringGen.sample.get, null);
            }
          }else if(event.getChannel().isDirect()){
            // this is a direct message, so the bot should reply
            val message = event.getMessageContent().substring(session.sessionPersona().getId().length()+3);
            if(message == "blueJeans url"){
              session.sendMessage(event.getChannel(), """https://bluejeans.com/939552048/""", null);
            }else {
              session.sendMessage(event.getChannel(), arbStringGen.sample.get, null);
            }
          }else if(StringUtils.containsIgnoreCase(event.getMessageContent(),"<@" + session.sessionPersona().getId() + ">")){
            // this bot is mentioned, so it should say something
            session.sendMessage(event.getChannel(), "How can I help you? You can send messages to @" + session.sessionPersona().getUserName() +  " or open a direct message chat. For help, say '@" + session.sessionPersona().getUserName() +  "' or send a direct message with the text 'help'", null);
          }
        }
      }
    })
  }


  start

}
