from django.shortcuts import render, redirect
from django.db.models import Q
from django.urls import reverse
from django.http import HttpResponse
from django.contrib.auth import authenticate, login, logout
from what_the_book.models import Book, Review, User, Admin
from what_the_book.forms import BookRequestForm, ReviewForm, UserForm, UserPictureForm
from django.contrib.auth.decorators import login_required
from django.utils import timezone


def redirect_to_home(request):
    return redirect(reverse('what_the_book:home'))


def home(request):
    book_list = Book.objects.order_by('title')
    context_dict = {}
    context_dict['books'] = book_list
    return render(request, 'what_the_book/home.html', context=context_dict)


def contactus(request):
    return render(request, 'what_the_book/contactus.html', context={})


def show_book(request, book_name_slug):
    context_dict = {}
    try:
        book = Book.objects.get(
            Q(title__iexact=book_name_slug.replace('-', ' ')))
        reviews = Review.objects.filter(reviewOf=book)
    except Book.DoesNotExist:
        book = None
        reviews = Review.objects.none()

    context_dict['book'] = book
    context_dict['reviews'] = reviews
    return render(request, 'what_the_book/book.html', context_dict)


def request_book(request):
    form = BookRequestForm()

    if request.method == 'POST':
        form = BookRequestForm()
        if form.is_valid():
            form.save(commit=True)

            return redirect('/home/')
        else:
            print(form.errors)

    return render(request, 'what_the_book/request_book.html', {'form': form})


@login_required
def make_review(request, book_name_slug):

    if request.user.is_authenticated:
        username = request.user.username
    try:
        user = User.objects.get(username=username)
        reviewOf = Book.objects.get(slug=book_name_slug)

    except Book.DoesNotExist:
        reviewOf = None

    if reviewOf is None:
        return redirect('what_the_book')

    form = ReviewForm()

    if request.method == 'POST':
        form = ReviewForm(request.POST)

        if form.is_valid():
            review = form.save()
            review.reviewOf = reviewOf
            review.createdBy = user

            review.likes = 0
            review.createdOn = timezone.now()
            review.save()

            return redirect(reverse('what_the_book:show_book', kwargs={'book_name_slug': book_name_slug
                                                                       }))
        else:
            print(form.errors)

    context_dict = {'form': form, 'reviewOf': reviewOf,
                    'book_name_slug': book_name_slug}

    return render(request, 'what_the_book/make_review.html', context=context_dict)


def register(request):
    registered = False

    if request.method == 'POST':
        user_form = UserForm(request.POST)
        user_picture_form = UserPictureForm(request.POST)

        if user_form.is_valid() and user_picture_form.is_valid():
            user = user_form.save()
            user.set_password(user.password)
            user.save()

            picture_user = user_picture_form.save(commit=False)
            picture_user.username = user_form['username'].data
            picture_user.password = user_form['password'].data

            if 'profilePicture' in request.FILES:
                picture_user.profilePicture = request.FILES['profilePicture']

            picture_user.save()
            registered = True
        else:
            print(user_form.errors, user_picture_form.errors)
    else:
        user_form = UserForm()
        user_picture_form = UserPictureForm()

    return render(request, 'what_the_book/register.html', context={'user_form': user_form,
                                                                   'user_picture_form': user_picture_form,
                                                                   'registered': registered})


def user_login(request):
    if request.method == 'POST':
        username = request.POST.get('username')
        password = request.POST.get('password')

        user = authenticate(username=username, password=password)

        if user:
            if user.is_active:
                login(request, user)
                return redirect_to_home(request)
            else:
                return HttpResponse("Your account is disabled.")
        else:
            return HttpResponse("Invalid login details.")
    else:
        return render(request, 'what_the_book/login.html')


@login_required
def account(request):
    if request.user.is_authenticated:
        username = request.user.username
    try:
        user = User.objects.get(username=username)
        reviews = Review.objects.filter(createdBy=user)
    except User.DoesNotExist:
        user = None
        reviews = None
    except Review.DoesNotExist:
        reviews = None

    context_dict = {'user': user, 'reviews': reviews}
    return render(request, 'what_the_book/account.html', context=context_dict)


@login_required
def user_logout(request):
    logout(request)
    return redirect_to_home(request)
