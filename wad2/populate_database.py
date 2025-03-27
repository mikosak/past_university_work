from datetime import date
import django
import os
os.environ.setdefault('DJANGO_SETTINGS_MODULE',
                      'what_the_book_project.settings')
django.setup()

# only import models after setup
from what_the_book.models import *


def populate():

    users = [
        {
            'username': 'John Dough',
            'password': '1234567890'},
        {
            'username': 'Jane Pastry',
            'password': 'incorrect'},
        {
            'username': 'ILOVEREADING3275',
            'password': 'themostincrediblysecurepassword'},
    ]

    admins = [
        {
            'username': 'theenforcer',
            'password': 'password'},
        {
            'username': 'big jim',
            'password': 'xG8Wl*g6Fd'},
    ]

    books = [
        {
            'title': 'Romeo and Juliet',
            'author': 'William Shakespeare',
            'coverPicture':'images/book_covers/romeo_and_juliet.jpg'},
        {
            'title': 'Macbeth',
            'author': 'William Shakespeare',
            'coverPicture':'images/book_covers/macbeth.jpg'},
        {
            'title': 'Othello',
            'author': 'William Shakespeare',
            'coverPicture':'images/book_covers/othello.jpg'},
        {
            'title': 'King Lear',
            'author': 'William Shakespeare',
            'coverPicture':'images/book_covers/king-lear.jpg'},




    ]

    requests = [
        {
            'title': 'The Hobbit',
            'author': 'J.R.R. Tolkien'},
        {
            'title': 'The Very Hungry Caterpillar',
            'author': 'Eric Carle'},
    ]

    reviews = [
        {
            'title': 'lack of modernity',
            'mainText': 'a large body of text may be fit into a text field',
            'createdOn': date(2022, 3, 3),
            'likes': 12},
        {
            'title': 'macbeth? more like macsucks',
            'mainText': 'a deep and scathing review',
            'createdOn': date(2013, 7, 19),
            'likes': 527},
    ]
    
    # Helper functions

    def add_user(username, password):
        user = User.objects.get_or_create(
            username=username, password=password)[0]
        user.save()
        return user

    def add_admin(username, password):
        admin = Admin.objects.get_or_create(
            username=username, password=password)[0]
        admin.save()
        return admin

    def add_book(addedBy, title, author,coverPicture):
        book = Book.objects.get_or_create(
            title=title, author=author, addedBy=addedBy,coverPicture=coverPicture)[0]
        book.save()
        return book

    def add_request(requestedBy, readBy, title, author):
        request = BookToRequest.objects.get_or_create(
            title=title, author=author)[0]
        request.requestedBy = requestedBy
        request.readBy = readBy
        request.save()
        return request

    def add_review(reviewOf, createdBy, title, mainText, createdOn, likes):
        review = Review.objects.get_or_create(
            title=title, mainText=mainText, createdOn=createdOn, likes=likes)[0]
        review.reviewOf = reviewOf
        review.createdBy = createdBy
        review.save()
        return review

    foreign_values = {
        'users': [],
        'admins': [],
        'books': [],
    }

    # Populate models

    for item in admins:
        admin = add_admin(**item)
        foreign_values['admins'].append(admin)

    for item in users:
        user = add_user(**item)
        foreign_values['users'].append(user)

    for index, item in enumerate(books):
        book = add_book(foreign_values['admins'][index % len(
            foreign_values['admins'])], **item)
        foreign_values['books'].append(book)

    for index, item in enumerate(requests):
        request = add_request(
            foreign_values['users'][index], foreign_values['admins'][index], **item)

    for index, item in enumerate(reviews):
        review = add_review(
            foreign_values['books'][index], foreign_values['users'][index], **item)

    # Print out population data

    print("\nUsers:")
    for user in User.objects.all():
        print(user)

    print("\nAdmins:")
    for admin in Admin.objects.all():
        print(admin)

    print("\nBooks:")
    for book in Book.objects.all():
        print(book)

    print("\nRequests")
    for request in BookToRequest.objects.all():
        print(request)

    print("\nReviews:")
    for review in Review.objects.all():
        print(review)

# Main


if __name__ == '__main__':
    print('Starting book population script...')
    populate()
