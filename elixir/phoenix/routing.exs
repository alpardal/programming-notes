defmodule PhoenixRouting do
  def current_url_settings do
    RumblWeb.Endpoint.struct_url()
  end

  def testing_routing_helpers do
    alias RumblWeb.Router.Helpers, as: Routes

    IO.inspect Routes.video_url(%URL{}, :show, video_struct)

    # using a domain + prefix:
    url = URI.parse("http://some-domain/prefix")
    IO.inspect Routes.video_url(url, :show, video_struct)

    # using current app url settings:
    IO.inspect Routes.video_url(current_url_settings(), :show, video_struct)
  end
end
