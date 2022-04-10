class Post < ApplicationRecord
  validates :title, length: { minimum: 2 }, on: :create
  validates :author, length: { minimum: 2 }, on: :special_context
  # post.save(context: :special_context)
end
