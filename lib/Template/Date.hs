module Template.Date where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Database.Persist.Sql (fromSqlKey)
import Foundation
import Model
import Yesod

data StagedItem = StagedItem
  { siName        :: Text
  , siDescription :: Maybe Text
  , siLocation    :: Maybe Text
  , siStart       :: Text
  , siEnd         :: Text
  }

stagedItineraryFragment :: [StagedItem] -> HtmlUrl (Route App)
stagedItineraryFragment items = [hamlet|
  <div id="itinerary-staging">
    $forall (i, item) <- indexed
      <input type="hidden" name="item-name-#{i}" value="#{siName item}">
      <input type="hidden" name="item-start-#{i}" value="#{siStart item}">
      <input type="hidden" name="item-end-#{i}" value="#{siEnd item}">
      $maybe desc <- siDescription item
        <input type="hidden" name="item-description-#{i}" value="#{desc}">
      $maybe loc <- siLocation item
        <input type="hidden" name="item-location-#{i}" value="#{loc}">
    $if null items
      <p class="text-sm text-gray-500 mb-2">Ingen aktivitetar enno.
    $else
      <ul class="flex flex-col gap-2 list-none mb-3">
        $forall (i, item) <- indexed
          <li class="flex items-center justify-between border rounded-lg px-3 py-2 text-sm">
            <span>
              #{siName item}
              <span class="text-gray-400 text-xs ml-2">#{siStart item} – #{siEnd item}
            <button type="button" class="text-sm border rounded px-2 py-1 cursor-pointer font-[inherit] bg-red-600 text-white"
              hx-post=@{DateNewItineraryItemByIntR i}
              hx-target="#itinerary-staging"
              hx-swap="outerHTML"
              hx-include="#itinerary-staging input[type=hidden]">Fjern
    <div class="flex flex-col gap-2 border-t pt-3">
      <input type="text" name="item-name" placeholder="Namn på aktivitet" class="w-full px-3 py-2 border rounded-lg text-sm font-[inherit]">
      <input type="text" name="item-description" placeholder="Skildring (valfri)" class="w-full px-3 py-2 border rounded-lg text-sm font-[inherit]">
      <input type="text" name="item-location" placeholder="Stad (valfri)" class="w-full px-3 py-2 border rounded-lg text-sm font-[inherit]">
      <div class="flex gap-2">
        <div class="flex flex-col gap-1 flex-1">
          <label class="text-xs text-gray-500">Start
          <input type="datetime-local" name="item-start" class="w-full px-3 py-2 border rounded-lg text-sm font-[inherit]">
        <div class="flex flex-col gap-1 flex-1">
          <label class="text-xs text-gray-500">Slutt
          <input type="datetime-local" name="item-end" class="w-full px-3 py-2 border rounded-lg text-sm font-[inherit]">
      <button type="button" class="px-3 py-2 rounded-lg text-sm font-semibold font-[inherit] border-0 cursor-pointer bg-green-600 text-white"
        hx-post=@{DateNewItineraryItemR}
        hx-target="#itinerary-staging"
        hx-swap="outerHTML"
        hx-include="#itinerary-staging">Legg til aktivitet
|]
  where
    indexed = zip [(0 :: Int) ..] items

itineraryFragment :: Int64 -> [Entity ItineraryItem] -> HtmlUrl (Route App)
itineraryFragment rawId items =
  [hamlet|
    <section id="itinerary-section">
      <h2 class="text-base font-semibold mb-3">Program
      $if null items
        <p class="text-sm text-gray-500">Ingen aktivitetar enno.
      $else
        <ul class="flex flex-col gap-3 list-none mb-4">
          $forall Entity itemId item <- items
            <li class="border rounded-lg p-3 flex items-start justify-between gap-4">
              <div>
                <p class="font-medium text-sm">#{itineraryItemName item}
                $maybe desc <- itineraryItemDescription item
                  <p class="text-sm text-gray-500">#{desc}
                $maybe loc <- itineraryItemLocation item
                  <p class="text-sm text-gray-400">#{loc}
                <p class="text-xs text-gray-400">#{fmtTime (itineraryItemStart item)} – #{fmtTime (itineraryItemEnd item)}
              <button class="text-sm border rounded px-2 py-1 cursor-pointer font-[inherit] bg-red-600 text-white"
                hx-post=@{DateByInt64ItineraryItemByInt64DeleteR rawId (fromSqlKey itemId)}
                hx-target="#itinerary-section"
                hx-swap="outerHTML"
                hx-confirm="Fjern aktivitet?">Fjern
      <form class="flex flex-col gap-2 mt-4 border-t pt-4"
            hx-post=@{DateByInt64ItineraryItemR rawId}
            hx-target="#itinerary-section"
            hx-swap="outerHTML">
        <p class="text-sm font-semibold">Legg til aktivitet
        <input type="text" name="name" required placeholder="Namn" class="w-full px-3 py-2 border rounded-lg text-sm font-[inherit]">
        <input type="text" name="description" placeholder="Skildring (valfri)" class="w-full px-3 py-2 border rounded-lg text-sm font-[inherit]">
        <input type="text" name="location" placeholder="Stad (valfri)" class="w-full px-3 py-2 border rounded-lg text-sm font-[inherit]">
        <div class="flex gap-2">
          <div class="flex flex-col gap-1 flex-1">
            <label class="text-xs text-gray-500">Start
            <input type="datetime-local" name="start" required class="w-full px-3 py-2 border rounded-lg text-sm font-[inherit]">
          <div class="flex flex-col gap-1 flex-1">
            <label class="text-xs text-gray-500">Slutt
            <input type="datetime-local" name="end" required class="w-full px-3 py-2 border rounded-lg text-sm font-[inherit]">
        <button type="submit" class="px-3 py-2 rounded-lg text-sm font-semibold font-[inherit] border-0 cursor-pointer bg-green-600 text-white">Legg til
  |]
  where
    fmtTime :: UTCTime -> Text
    fmtTime = T.pack . formatTime defaultTimeLocale "%d.%m %H:%M"

dateForm :: Maybe DateIdea -> Maybe DateIdeaId -> Widget
dateForm mIdea mDid =
  [whamlet|
    <form class="flex flex-col gap-4 max-w-lg mx-auto" method="post" enctype="multipart/form-data">
      <label class="block text-sm font-semibold mb-1.5">Tittel
      <input type="text" name="title" value="#{maybe "" dateIdeaTitle mIdea}" required class="w-full px-3.5 py-2.5 border rounded-lg text-base font-[inherit] transition">
      <label class="block text-sm font-semibold mb-1.5">Skildring
      <textarea name="description" class="w-full px-3.5 py-2.5 border rounded-lg text-base font-[inherit] transition min-h-28 resize-y leading-normal">#{maybe "" dateIdeaDescription mIdea}
      <label class="block text-sm font-semibold mb-1.5">Stad
      <input type="text" name="location" value="#{maybe "" (maybe "" id . dateIdeaLocation) mIdea}" class="w-full px-3.5 py-2.5 border rounded-lg text-base font-[inherit] transition">
      <label class="block text-sm font-semibold mb-1.5">Status
      <select name="status" class="w-full px-3.5 py-2.5 border rounded-lg text-base font-[inherit] transition">
        $forall (val, lbl) <- dateStatusOptions
          <option value="#{val}" :currentStatus == val:selected>#{lbl}
      <label class="block text-sm font-semibold mb-1.5">Bilete
      <input type="file" name="images" accept="image/*" multiple class="w-full px-3 py-2 text-sm font-[inherit] border rounded-lg cursor-pointer">
      <div class="flex items-center gap-4">
        <button type="submit" class="px-3 py-2 rounded-lg text-base font-semibold font-[inherit] border-0 cursor-pointer shadow-sm transition bg-green-600 text-white">Lagre
        $maybe did <- mDid
          <button class="px-3 py-2 rounded-lg text-base font-semibold font-[inherit] border-0 cursor-pointer transition bg-red-600 text-white"
            hx-post=@{DateByInt64DeleteR (fromSqlKey did)}
            hx-confirm="Slett date?">Slett date
        <a href=@{DateR} class="text-sm font-medium no-underline text-red-600">Avbryt
  |]
  where
    currentStatus = maybe "idea" (dateStatusVal . dateIdeaStatus) mIdea

statusLabel :: DateStatus -> Text
statusLabel Idea = "Idé"
statusLabel Planned = "Planlagt"
statusLabel Done = "Gjennomført"

dateStatusOptions :: [(Text, Text)]
dateStatusOptions =
  [ ("idea", "Idé"),
    ("planned", "Planlagt"),
    ("done", "Gjennomført")
  ]

dateStatusVal :: DateStatus -> Text
dateStatusVal Idea = "idea"
dateStatusVal Planned = "planned"
dateStatusVal Done = "done"
