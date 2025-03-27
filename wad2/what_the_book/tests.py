from datetime import datetime, timedelta, date
from django.test import TestCase
from django.shortcuts import reverse
from what_the_book.models import *


class ModelTests(TestCase):
    def test_likes_non_negative(self):
        review = Review(
            title="Title", createdOn=datetime.now().date(), likes=-1)
        review.save()

        self.assertEqual((review.likes >= 0), True)

    def test_valid_date(self):
        review = Review(
            title="Title", createdOn=datetime.now().date() + timedelta(days=1))
        review.save()
        self.assertEqual((review.createdOn <= datetime.now().date()), True)

    def test_valid_book_slug(self):
        book = Book(title="A BOOK ABOUT SHOUTING")
        book.save()
        self.assertEqual((book.slug == "a-book-about-shouting"), True)


class ViewTests(TestCase):
    def test_home_with_no_books(self):
        response = self.client.get(reverse('what_the_book:home'))
        self.assertEqual(response.status_code, 200)
        self.assertContains(response, 'No books found.')
        self.assertQuerysetEqual(response.context['books'], [])

    def test_home_with_book(self):
        add_book("Book about testing", "The tester")
        response = self.client.get(reverse('what_the_book:home'))
        self.assertEqual(response.status_code, 200)
        self.assertContains(response, "Book about testing")
        number_of_books = len(response.context['books'])
        self.assertEqual(number_of_books, 1)


def add_book(title, author):
    book = Book.objects.get_or_create(title=title)[0]
    book.title = title
    book.author = author
    book.save()
    return book
