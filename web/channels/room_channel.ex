defmodule Chat.RoomChannel do
  use Phoenix.Channel

  intercept ["new_msg"]

  def join("room:lobby", _message, socket) do
    {:ok, socket}
  end

  def join("room:" <> _private_room_id, _params, _socket) do
    {:error, %{reason: "unauthorized"}}
  end

  def handle_in("new:msg", %{"body" => body}, socket) do
    broadcast! socket, "new:msg", %{body: body}
    {:noreply, socket}
  end

  def handle_out("new:msg", payload, socket) do
    push socket, "new:msg", payload
    {:noreply, socket}
  end
end
